(ns plexus.concavity-test
  (:require [#?(:clj clojure.test :cljs cljs.test) :refer [deftest is testing]]
            [clj-manifold3d.core :as m]
            [plexus.core :as p]
            [plexus.surface :as s]
            [plexus.surface-test :refer [near? near-vector? failure]]))

(defn valley []
  (m/extrude (m/cross-section [[[-10 -5] [10 -5] [10 10] [0 0] [-10 10]]]) 10))
(defn trench []
  (m/extrude (m/cross-section [[[-10 -5] [10 -5] [10 10] [0.1 10]
                               [0.1 0] [-0.1 0] [-0.1 10] [-10 10]]]) 10))
(defn bowl []
  (m/difference (m/cube 24 24 12 true) (m/translate (m/sphere 10 48) [0 0 5])))
(defn torus [] (m/revolve (m/translate (m/circle 3 48) [8 0]) 96))
(def valley-distance (* 8 (#?(:clj Math/sqrt :cljs js/Math.sqrt) 2)))
(defn volume [g] (:volume (m/get-properties g)))
(defn sweep [surface origin direction distance depth step]
  (p/extrude
    (p/frame :name :path :cross-section (m/square 0.2 0.2 true))
    (p/on-surface :surface surface :origin origin :direction direction :depth depth :step-size step)
    (p/forward :length distance)))

(defn copied-mesh [g]
  (let [mesh (m/get-mesh g)]
    (try (m/mesh-data mesh) (finally #?(:clj (.close mesh) :cljs (m/dispose! mesh))))))

(defn- pierced? [a b [p q r]]
  ;; Independent segment/triangle intersection check, excluding boundary contact.
  (let [u (s/sub q p) v (s/sub r p) normal (s/cross u v)
        da (s/dot (s/sub a p) normal) db (s/dot (s/sub b p) normal)]
    (when (and (< (* da db) 0) (> (s/abs (- da db)) 1e-14))
      (let [x (s/sub (s/add a (s/scale (s/sub b a) (/ da (- da db)))) p)
            uu (s/dot u u) uv (s/dot u v) vv (s/dot v v)
            xu (s/dot x u) xv (s/dot x v)
            denominator (- (* uu vv) (* uv uv))]
        (when (> denominator 1e-20)
          (let [bary-u (/ (- (* vv xu) (* uv xv)) denominator)
                bary-v (/ (- (* uu xv) (* uv xu)) denominator)]
            (and (> bary-u 1e-7) (> bary-v 1e-7) (< (+ bary-u bary-v) (- 1 1e-7)))))))))

(defn proper-intersections
  "Count transverse intersections of nonadjacent output triangles. Native status
  checks topology; this independent geometric check detects the original fold."
  [g]
  (let [{:keys [num-prop vert-properties tri-verts]} (copied-mesh g)
        vertices (mapv #(vec (take 3 %)) (partition num-prop vert-properties))
        triangles (vec (sort-by #(get-in % [:lo 0])
                                (map (fn [ids]
                                       (let [points (mapv vertices ids)]
                                         {:points points :ids (set ids)
                                          :lo (apply mapv min points) :hi (apply mapv max points)})) tri-verts)))
        edges (fn [[a b c]] [[a b] [b c] [c a]])]
    (count
      (for [i (range (count triangles))
            :let [a (triangles i)]
            j (range (inc i) (count triangles))
            :let [b (triangles j)]
            :while (<= (get-in b [:lo 0]) (get-in a [:hi 0]))
            :when (and (not-any? (:ids a) (:ids b))
                       (every? (fn [axis] (and (<= (get-in a [:lo axis]) (get-in b [:hi axis]))
                                               (<= (get-in b [:lo axis]) (get-in a [:hi axis])))) [1 2])
                       (or (some (fn [[p q]] (pierced? p q (:points b))) (edges (:points a)))
                           (some (fn [[p q]] (pierced? p q (:points a))) (edges (:points b)))))]
        [i j]))))

(defn check-path [cursor x z]
  (let [path (s/walk cursor x z) end (peek path)
        back (peek (s/walk end (- x) (- z)))
        expected (#?(:clj Math/sqrt :cljs js/Math.sqrt) (+ (* x x) (* z z)))
        measured (reduce + (map #(s/length (s/sub (:point %2) (:point %1))) path (next path)))]
    (is (near? expected measured))
    (is (near-vector? (:point cursor) (:point back)))
    (is (near-vector? (:forward cursor) (:forward back)))
    (is (every? (fn [c]
                  (let [normal (s/normal c)
                        bary (s/bary (:surface c) (:face c) (:point c))]
                    (and (< (s/abs (s/dot normal (:forward c))) 1e-7)
                         (near? 1 (s/length (:forward c)))
                         (every? #(<= -1e-6 % 1.000001) bary)))) path))
    end))

(deftest concave-valley-has-an-analytic-endpoint
  (let [surface (p/surface (valley))
        cursor (s/attach surface {:origin [-4 4 3.17] :direction [1 -1 0]})]
    (doseq [side [0 2 -2]]
      (let [end (check-path cursor side valley-distance)]
        (is (near-vector? [4 4 (- 3.17 side)] (:point end)))
        (is (near-vector? (s/unit [-1 1 0]) (s/normal end)))
        (is (near-vector? (s/unit [1 1 0]) (:forward end)))))
    (doseq [steps [1 17 113]]
      (is (near-vector? [4 4 3.17] (:point (peek (s/sample cursor :z valley-distance steps))))))))

(deftest narrow-connected-trench-does-not-jump-between-facing-walls
  (let [cursor (s/attach (p/surface (trench)) {:origin [-0.1 7 3.17] :direction [0 -1 0]})
        end (check-path cursor 0 12.2)
        path (s/walk cursor 0 12.2)]
    (is (near-vector? [0.1 5 3.17] (:point end)))
    (is (near-vector? [-1 0 0] (s/normal end)))
    (is (near-vector? [0 1 0] (:forward end)))
    (is (some #(near? 0 (get-in % [:point 1])) path))))

(deftest concave-sweeps-have-clearance-and-no-transverse-intersections
  (doseq [[label geometry origin direction distance]
          [[:bowl (bowl) [1.3 0.7 -4.8] [1 0.21 0.11] 8]
           [:torus (torus) [5.2 0.7 0.2] [0.17 1 0.19] 12]]]
    (let [surface (p/surface geometry)
          cursor (s/attach surface {:origin origin :direction direction})]
      (check-path cursor 0 distance)
      (doseq [depth [-0.5 0 0.5] step [1 0.1]]
        (testing (str label " depth=" depth " spacing=" step)
          (let [model (sweep surface origin direction distance depth step)
                g (p/get-model model :path)
                v (volume g)
                inside (volume (m/intersection g geometry))
                outside (volume (m/difference g geometry))]
            (is (= :NoError (m/status g)))
            (is (< (* 0.8 0.04 distance) v (* 1.2 0.04 distance)))
            (is (near? v (+ inside outside)))
            (is (zero? (proper-intersections g)))
            (cond
              (pos? depth) (is (< (s/abs inside) 1e-6))
              (neg? depth) (is (< (s/abs outside) 1e-6))
              :else (is (and (pos? inside) (pos? outside))))))))))

(deftest native-status-misses-sharp-concave-sweep-intersections
  (let [surface (p/surface (valley))
        cursor (s/attach surface {:origin [-4 4 3.17] :direction [1 -1 0] :depth 0.5 :step-size 0.1})
        samples (s/sample cursor :z valley-distance nil)
        profile (m/square 0.2 0.2 true)
        ;; Reproduce the old unchecked loft to verify the geometric checker.
        unchecked (m/loft (mapv #(hash-map :cross-section profile :frame (s/frame %)) samples))]
    (is (= :NoError (m/status unchecked)))
    (is (pos? (proper-intersections unchecked)))
    (doseq [depth [0 0.5] step [1 0.1]]
      (let [error (failure #(sweep surface [-4 4 3.17] [1 -1 0] valley-distance depth step))]
        (is (= :surface-sweep-fold (:reason error)))
        (is (= :plexus.impl/forward (:key error)))))
    ;; A below-surface profile on the outside of this turn does not fold.
    (let [g (p/get-model (sweep surface [-4 4 3.17] [1 -1 0] valley-distance -0.5 0.1) :path)]
      (is (zero? (proper-intersections g)))
      (is (< (s/abs (volume (m/difference g (valley)))) 1e-6)))))

(deftest smooth-concave-normals-and-excessive-offset
  (let [surface (p/surface (bowl))
        cursor (s/attach surface {:origin [1.3 0.7 -4.8] :direction [1 0.21 0.11]})
        path (s/walk cursor 0 8)
        crossings (filter (fn [[a b]] (not= (:face a) (:face b))) (partition 2 1 path))]
    (is (seq crossings))
    (is (every? (fn [[a b]] (near-vector? (s/frame-normal a)
                                          (s/frame-normal (assoc a :face (:face b))))) crossings))
    (is (every? #(> (s/dot (s/frame-normal %) (s/unit (s/sub [0 0 5] (:point %)))) 0.995) path))
    (is (= :surface-sweep-fold
           (:reason (failure #(sweep surface [1.3 0.7 -4.8] [1 0.21 0.11] 8 11 0.1)))))))
