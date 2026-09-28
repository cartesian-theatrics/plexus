;; Run: clojure -M:dev src/examples/readme_surface_navigation.clj
(require
 '[clj-manifold3d.core :as m]
 '[plexus.core
   :refer [result frame forward extrude branch rotate translate union export
           surface on-surface]])

(let [support (m/cylinder 30 20 20 96)]
  (-> (extrude
       (result :name :model :expr (union support :ribbon))
       (frame :name :ribbon :cross-section (m/square 1.2 0.6 true))
       (on-surface :surface (surface support)
                   :origin [20 0 8.37] :direction [0 1 0.15]
                   :depth 0.45 :step-size 0.5)

       (forward :length 65)
       (branch :from :ribbon
               (rotate :y -0.7)
               (forward :length 12))
       (rotate :y 0.7)
       (forward :length 12)
       (translate :y 1)
       (forward :length 4))
      (export "surface-navigation.glb"
              (m/material :color [0 0.7 0.7 1.0] :metalness 0.2))))
