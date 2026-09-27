(ns plexus.surface-test
  (:require [#?(:clj clojure.test :cljs cljs.test) :refer [deftest is]]
            [clj-manifold3d.core :as m]
            [plexus.core :as p]
            [plexus.surface :as s]
            [plexus.transforms :as tf]))

(defn near? [a b] (< (s/abs (- a b)) 1e-5))
(defn near-vector? [a b] (every? true? (map near? a b)))
(defn endpoint [model name] (tf/translation-vector (p/lookup-transform model name)))
(defn failure [f] (try (f) nil (catch #?(:clj Exception :cljs :default) e (ex-data e))))
(defn box-surface [] (p/surface (m/cube 20 20 10)))
(defn start [surface] (p/on-surface :surface surface :origin [2 3 10] :direction [1 0 0]))

(deftest surface-coordinates-depth-and-detachment
  (let [model (p/extrude
                (p/frame :name :path)
                (start (box-surface))
                (p/forward :length 4)
                (p/save-transform :frame :path :name :forward)
                (p/translate :x 2 :z 1 :y 1)
                (p/save-transform :frame :path :name :translated)
                (p/rotate :y (/ #?(:clj Math/PI :cljs js/Math.PI) 2))
                (p/forward :length 2)
                (p/save-transform :frame :path :name :turned)
                (p/translate :y -2)
                (p/save-transform :frame :path :name :below)
                (p/off-surface)
                (p/rotate :x 0.4)
                (p/forward :length 2))]
    (is (near-vector? [6 3 10] (endpoint model :forward)))
    (is (near-vector? [7 5 11] (endpoint model :translated)))
    (is (near-vector? [7 7 11] (endpoint model :turned)))
    (is (near-vector? [7 7 9] (endpoint model :below)))
    (is (nil? (get-in model [:frames :path :surface-cursor])))))

(deftest surface-sweeps-signed-axes-centering-and-gaps
  (doseq [[axis direction] [[:length [1 0 0]] [:x [0 1 0]] [:y [0 0 1]]]
          distance [2 -2]]
    (let [model (p/extrude
                  (p/frame :name :path :cross-section (m/square 0.2 0.2 true))
                  (p/on-surface :surface (box-surface) :origin [5 5 10] :direction [1 0 0])
                  (p/forward {axis distance})
                  (p/save-transform :frame :path :name :end))]
      (is (near-vector? (s/add [5 5 10] (s/scale direction distance)) (endpoint model :end)))
      (is (near? 0.08 (:volume (m/get-properties (p/get-model model :path)))))))
  (let [model (p/extrude
                (p/frame :name :path :cross-section (m/square 1 1))
                (start (box-surface))
                (p/forward :length 2 :center true)
                (p/forward :length 2 :branch? true)
                (p/forward :length 3 :gap true)
                (p/save-transform :frame :path :name :end))]
    (is (near-vector? [6 3 10] (endpoint model :end)))
    (is (near? 4 (:volume (m/get-properties (p/get-model model :path)))))))

(deftest cylinder-distance-parallel-transport-and-bent-geometry
  (let [surface (p/surface (m/cylinder 10 10 10 32))
        cursor (s/attach surface {:origin [9.9 1 3.17] :direction [0 1 0]})
        circumference (* 32 20 (#?(:clj Math/sin :cljs js/Math.sin) (/ #?(:clj Math/PI :cljs js/Math.PI) 32)))
        path (s/walk cursor 0 circumference)
        end (peek path)
        back (peek (s/walk end 0 (- circumference)))
        measured (reduce + (map #(s/length (s/sub (:point %2) (:point %1))) path (next path)))
        model (p/extrude
                (p/frame :name :path :cross-section (m/square 0.2 0.2 true))
                (p/on-surface :surface surface :origin [10 0 3.17] :direction [0 1 0])
                (p/forward :length 20 :n-steps 1)
                (p/save-transform :frame :path :name :end))]
    (is (> (count path) 32))
    (is (near? circumference measured))
    (is (near-vector? (:point cursor) (:point end)))
    (is (near-vector? (:forward cursor) (:forward end)))
    (is (near-vector? (:point cursor) (:point back)))
    (is (near-vector? (:forward cursor) (:forward back)))
    (is (> (count (get-in model [:frames :path :segments 0 :all-transforms])) 10))
    (is (< -5 (first (endpoint model :end)) -3))
    (is (< 0.75 (:volume (m/get-properties (p/get-model model :path))) 0.85))))

(deftest surface-branches-inheritance-targeting-and-rebuild
  (let [distance (atom 2)
        model (p/extrude
                (p/frame :name :base)
                (p/translate :x 40)
                (start (box-surface))
                (p/frame :name :other)
                (p/branch :from :base :with [:base]
                  (p/forward :length 4)
                  (p/save-transform :frame :base :name :branch))
                (p/deferred #(p/forward :length @distance :to [:base]))
                (p/save-transform :frame :base :name :base)
                (p/save-transform :frame :other :name :other))]
    (is (near-vector? [6 3 10] (endpoint model :branch)))
    (is (near-vector? [4 3 10] (endpoint model :base)))
    (is (near-vector? [2 3 10] (endpoint model :other)))
    (reset! distance 3)
    (is (near-vector? [5 3 10] (endpoint (p/rebuild model) :base)))))

(deftest topology-seams-and-disconnected-nearby-sheets
  (let [surface (p/surface (m/calculate-normals (m/cube 10 10 10) 0 52.5))
        cursor (s/attach surface {:origin [2 3 10] :direction [1 0 0]})
        end (peek (s/walk cursor 0 11))]
    (is (near-vector? [10 3 7] (:point end))))
  (let [surface (p/surface (m/union [(m/cube 10 10 10)
                                    (m/translate (m/cube 10 10 10) [10.01 0 0])]))
        cursor (s/attach surface {:origin [8 3 10] :direction [1 0 0]})
        end (peek (s/walk cursor 0 5))]
    (is (near-vector? [10 3 7] (:point end)))
    (is (near-vector? [1 0 0] (s/normal end)))))

(deftest ambiguous-vertices-and-unsupported-motion-fail-explicitly
  (let [surface (box-surface)
        cursor (s/attach surface {:origin [19 19 10] :direction [1 1 0]})]
    (is (= :vertex-singularity (:reason (failure #(s/walk cursor 0 4)))))
    (doseq [op [(p/rotate :x 1) (p/translate :global? true :x 2)
                (p/transform :replace (m/frame)) (p/left :angle 1 :curve-radius 3)
                (p/forward :length 2 :twist 1)]]
      (is (:key (failure #(p/extrude (p/frame :name :path) (start surface) op)))))
    (is (failure #(s/attach surface {:origin [1 1 10] :direction [0 0 1]})))
    (is (failure #(p/on-surface :surface surface :origin [1 1 10] :direction [1 0 0] :step-size 0)))))

(deftest sphere-tracing-reversibility-and-depth
  (let [surface (p/surface (m/sphere 10 32))
        vertices (:vertices (first (:faces surface)))
        origin (s/scale (reduce s/add (map (:positions surface) vertices)) (/ 3))
        cursor (s/attach surface {:origin origin :direction [0.31 0.79 -0.47] :depth -0.2})
        path (s/walk cursor 0 17)
        end (peek path)
        back (peek (s/walk end 0 -17))
        sampled (peek (s/sample cursor :z 17 100))]
    (is (near-vector? (:point cursor) (:point back)))
    (is (near-vector? (:forward cursor) (:forward back)))
    (is (near-vector? (:point end) (:point sampled)))
    (is (near-vector? (:forward end) (:forward sampled)))
    (is (every? #(< 9.7 (s/length (:point %)) 10.00001) path))
    (is (near-vector? (s/add (:point end) (s/scale (s/frame-normal end) -0.2))
                      (tf/translation-vector (s/frame end))))))

(deftest surface-loft-and-world-space-points
  (let [surface (box-surface)
        model (p/extrude
                (p/frame :name :path :cross-section (m/square 1 1 true))
                (start surface)
                (p/loft
                  (p/forward :length 2)
                  (p/set :cross-section (m/square 2 2 true))
                  (p/forward :length 2)))]
    (is (< 6 (:volume (m/get-properties (p/get-model model :path))) 10)))
  (let [points (p/points :axes [:x :y :z]
                        (p/translate :x 40)
                        (p/frame :name :path)
                        (start (box-surface))
                        (p/forward :length 2))]
    (is (near-vector? [2 3 10] (first points)))
    (is (near-vector? [4 3 10] (last points)))))

(deftest can-start-at-a-sphere-pole
  (let [surface (p/surface (m/sphere 10 32))
        cursor (s/attach surface {:origin [0 0 10] :direction [1 0.37 0]})
        path (s/walk cursor 0 3)]
    (is (> (count path) 1))
    (is (near-vector? [0 0 10] (:point cursor)))
    (is (< (get-in (peek path) [:point 2]) 10))))

(deftest concave-surface-and-hollow-profile
  (let [torus (m/revolve (m/translate (m/circle 3 32) [8 0]) 64)
        surface (p/surface torus)
        cursor (s/attach surface {:origin [5.2 0.7 0.2] :direction [0.17 1 0.19]})
        path (s/walk cursor 0 12)
        end (peek path)
        back (peek (s/walk end 0 -12))]
    (is (> (count path) 10))
    (is (near-vector? (:point cursor) (:point back)))
    (is (near-vector? (:forward cursor) (:forward back))))
  (let [profile (m/difference (m/square 2 2 true) (m/square 1 1 true))
        model (p/extrude (p/frame :name :tube :cross-section profile)
                         (start (box-surface))
                         (p/forward :length 4))]
    (is (near? 12 (:volume (m/get-properties (p/get-model model :tube)))))))
