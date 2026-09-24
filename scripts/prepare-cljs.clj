(require '[clojure.java.io :as io])

;; Use the WASM binaries shipped with the resolved geometry dependency.
(doseq [name ["manifold.js" "manifold.wasm"]]
  (let [resource (io/resource (str "clj_manifold3d/wasm/" name))
        output (io/file "target/wasm" name)]
    (when-not resource
      (throw (ex-info "The geometry dependency must include WASM assets" {:name name})))
    (io/make-parents output)
    (with-open [input (io/input-stream resource)]
      (io/copy input output))))
(println "Prepared target/wasm from the resolved clj-manifold3d dependency.")
