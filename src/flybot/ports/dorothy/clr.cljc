(ns flybot.ports.dorothy.clr
  (:require [clojure.clr.io]
            [clojure.clr.shell :as sh])
  (:import [System.Diagnostics Process]
           [System.IO FileInfo]))

(defn- file-info [x]
  (cond
    (instance? FileInfo x) x
    (string? x) (try
                  (FileInfo. x)
                  (catch System.ArgumentException e))
    :else (throw (System.ArgumentException. "Expects FileInfo or String."))))

(defn- without-extension [path]
  (let [fi (file-info path)
        path' (.FullName fi)]
    (subs path' 0
      (- (count path')
         (count (.Extension fi))))))

(defn- make-temp-dot [path data]
  (let [fi (file-info
             (str (without-extension path) ".dot"))]
    (when (.Exists fi) (.Delete fi))
    (spit (.FullName fi) data)
    (.FullName fi)))

(defn- assoc-if-missing
  ([m k v]
   (if-not (contains? m k)
     (assoc m k v)
     m))
  ([m k v & vs]
   (reduce assoc-if-missing (assoc-if-missing m k v) vs)))

;; pass to show as :edge-label (attr-labeler g)
;; (defn attr-labeler [g opts]
;;   (fn [n1 n2]
;;     (fn [n1 n2]
;;       (or (la/attr g n1 n2 :label)
;;           (if-let [a (la/attrs g n1 n2)]
;;             (with-out-str (pprint/pprint a)))))))

(defn show
  ([g] (show g nil))
  ([g {:keys [alg path format]
       :or {alg "dot" ;; "/usr/local/bin/dot"
            format :png}
       :as opts}]
   (let [{:keys [path]
          :or {path (str "Temp/dotxamp."
                         (case format
                           :png "png"
                           :svg "svg"))}} opts
         path' (.FullName (file-info path))
         _ (println "opts: " opts)
         dotstr g ;;(apply lio/dot-str g (apply concat opts))
         temp-dot (make-temp-dot path' dotstr)
         format-arg (case format
                      :png "-Tpng"
                      :svg "-Tsvg")
         attempt (sh/sh alg format-arg temp-dot "-o" path')]
     (when (not= "" (:err attempt))
       (throw (Exception.
                (str "Some problem:\n"
                     (with-out-str
                       (clojure.pprint/pprint attempt))))))
     ;; doesn't work in windows:
     ;;(sh/sh "open" path')
     g)))


;; (defn render-to-bytes
;;   "Renders the graph g in the PNG format using GraphViz and returns PNG data
;;   as a byte array.
;;   Requires GraphViz's 'dot' (or a specified algorithm) to be installed in
;;   the shell's path. Possible algorithms include :dot, :neato, :fdp, :sfdp,
;;   :twopi, and :circo"
;;   [;; g & {:keys [alg] :or {alg "dot"} :as opts}
;;    dot]
;;   (let [;;dot (apply dot-str g (apply concat opts))
        
;;         {png :out} (sh/sh ;; (name alg)
;;                      "/usr/local/bin/dot"
;;                      "-Tpng -o ./flazzom.png" :in dot ;;:out-enc :bytes
;;                      )]
;;     png))



;; ============================================================
;; old port attempt before I got lazy

;;  (defn ^:private build-render-command [{:keys [format layout scale invert-y?]}]
;;   (->>
;;     [;;"dot"
;;      (if format    (str "-T" (name format)))
;;      (if layout    (str "-K" (name layout)))
;;      (if scale     (str "-s" scale))
;;      (if invert-y? "-y")]
;;     (remove nil?)))

;; (defn ^:private init-process-builder
;;   [{:keys [dir]
;;     :or {dir "."}
;;     :as options}]
;;   ;; (let [pb (java.lang.ProcessBuilder. ^java.util.List (build-render-command options))]
;;   ;;   (when dir (.directory pb (if (instance? java.io.File dir)
;;   ;;                              dir
;;   ;;                              (java.io.File. (str dir)))))
;;   ;;   pb)
;;   (let [p (new Process)]
;;     (set! (.. p StartInfo WorkingDirectory) dir)
;;     (set! (.. p StartInfo FileName) "dot") ;; maybe?
;;     ;; just dicking around now
;;     (set! (.. p StartInfo UseShellExecute) false)
;;     (set! (.. p StartInfo RedirectStandardInput) true)
;;     (set! (.. p StartInfo RedirectStandardOutput) true)
;;     ;; maaaaaybe? https://stackoverflow.com/questions/8826884/standardin-has-not-been-redirected-error-in-net-c
;;     (set! (.. p StartInfo Arguments)
;;       (clojure.string/join " " (build-render-command options)))
;;     p))

;; (def ^:private binary-formats
;;   #{:bmp :eps :gif :ico :jpg :jpeg :pdf :png :ps :ps2 :svgz :tif :tiff :vmlz :wbmp})

;; (defn ^:private read-dot-result [input-stream {:keys [format binary?]}]
;;   (if (or binary? (binary-formats format))
;;     (println "made it here")
;;     ;; (let [result (java.io.ByteArrayOutputStream.)]
;;     ;;   (jio/copy input-stream result)
;;     ;;   (.toByteArray result))
;;     ;; (slurp input-stream)
;;     ))

;; (defn render
;;   "Render the given graph (must be the string result of (dorothy.core/dot))
;;   using the Graphviz 'dot' tool. The 'dot' executable must be on the system
;;   path.

;;   Depending on the requested format (see options below), returns either a string
;;   or a Java byte array.

;;   options is a map with the following options:

;;     :dir       The working directory in which dot is executed. Defaults to '.'
;;     :format    The desired output format, e.g. :png, :svg. If the output format
;;                is known to be binary, a byte array is returned.
;;     :layout    Dot layout algorithm to use. (-K command-line option)
;;     :scale     Input scale, defaults to 72.0. (-s command-line option)
;;     :invert-y? If true, y coordinates in output are inverted. (-y command-line option)

;;   Examples:

;;     ; Simple 3 node graph, converted to dot and rendered as SVG.
;;     (-> (digraph [[:a :b :c]]) dot (render {:format :svg))

;;   See:

;;   * (dorothy.core/dot)
;;   * http://www.graphviz.org/content/command-line-invocation
;;   * http://www.graphviz.org/content/output-formats
;;   "
;;   [graph options]
;;   (let [p (init-process-builder options)
;;         _ (println (.. p StartInfo Arguments))
;;         _ (.Start p)
;;         from-dot (future (with-open [from-dot (.StandardInput p) ;;(.getInputStream p)
;;                                      ]
;;                            (read-dot-result from-dot options)))
;;         ]
;;     ;; (with-open [to-dot (.StandardOutput p) ;;(.getOutputStream p)
;;     ;;             ]
;;     ;;   (spit to-dot graph))
;;     ;;@from-dot
;;     ))

;; ;; (defn save!
;; ;;   "Render and save the given graph (string result of (dorothy.core/dot)) to an
;; ;;   output stream. f is any argument acceptable to (clojure.java.io/ouput-stream).

;; ;;   Examples:

;; ;;     ; Write a graph to a png file
;; ;;     (-> (digraph [[:a :b :c]])
;; ;;         dot
;; ;;         (save! \"out.png\" {:format :png}))

;; ;;   See:

;; ;;   * (dorothy.core/render)
;; ;;   * (dorothy.core/dot)
;; ;;   * http://clojure.github.com/clojure/clojure.java.io-api.html#clojure.java.io/make-output-stream
;; ;; "
;; ;;   [graph f & [options]]
;; ;;   (let [bytes (render graph (merge options {:binary? true}))]
;; ;;     (with-open [output (jio/output-stream f)]
;; ;;       (jio/copy bytes output)))
;; ;;   graph)
