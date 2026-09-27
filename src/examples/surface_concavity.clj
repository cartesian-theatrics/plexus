;; Run: clojure -M:dev src/examples/surface_concavity.clj
(require '[clj-manifold3d.core :as m]
         '[clojure.java.io :as io]
         '[plexus.core :as p])

(def cases
  (mapv
    (fn [[name support origin direction distance]]
      (let [ribbon (p/get-model
                    (p/extrude
                      (p/frame :name :ribbon :cross-section (m/square 0.2 0.2 true))
                      (p/on-surface :surface (p/surface support)
                                    :origin origin :direction direction :depth 0.5 :step-size 0.1)
                      (p/forward :length distance)) :ribbon)]
        {:name name :support support :ribbon ribbon}))
    [["bowl" (m/difference (m/cube 24 24 12 true)
                           (m/translate (m/sphere 10 48) [0 0 5]))
      [1.3 0.7 -4.8] [1 0.21 0.11] 8]
     ["torus" (m/revolve (m/translate (m/circle 3 48) [8 0]) 96)
      [5.2 0.7 0.2] [0.17 1 0.19] 12]]))

(doseq [{:keys [name support ribbon]} cases]
  (let [output (str "target/surface-concave-" name ".glb")
        scene (m/union (m/color support [0.28 0.34 0.42 1])
                       (m/color ribbon [1 0.45 0.08 1]))]
    (io/make-parents output)
    (m/export-mesh (m/get-mesh scene) output)
    (println "Wrote" output)))
