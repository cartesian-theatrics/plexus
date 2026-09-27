;; Run: clojure -M:dev src/examples/surface_navigation.clj
(require '[clj-manifold3d.core :as m]
         '[clojure.java.io :as io]
         '[plexus.core :as p]
         '[plexus.source :as ps])

(def support (m/cylinder 30 20 20 96))
(def surface (p/surface support))
(def ribbon
  (p/extrude
    (ps/frame :name :ribbon :cross-section (m/square 1.2 0.6 true))
    (ps/on-surface :surface surface
                   :origin [20 0 8.37] :direction [0 1 0.15]
                   :depth 0.45 :step-size 0.5)
    (ps/forward :length 65)
    (ps/branch :from :ribbon
      (ps/rotate :y -0.7)
      (ps/forward :length 12))
    (ps/rotate :y 0.7)
    (ps/forward :length 12)
    ;; Increasing depth moves away from the same surface point.
    (ps/translate :y 1)
    (ps/forward :length 8)
    (ps/save-transform :frame :ribbon :name :end)))

(def preview
  (m/union (m/color support [0.25 0.3 0.35 1])
           (m/color (p/get-model ribbon :ribbon) [1 0.45 0.08 1])))
(io/make-parents "target/surface-navigation.glb")
(m/export-mesh (m/get-mesh preview) "target/surface-navigation.glb")
(println "Wrote target/surface-navigation.glb")
