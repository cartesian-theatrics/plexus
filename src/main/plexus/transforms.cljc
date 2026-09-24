(ns plexus.transforms
  (:require [clj-manifold3d.core :as m])
  #?(:clj (:import [manifold3d.linalg MatrixTransforms DoubleMat3x4 DoubleVec3])))

(defn yaw [frame angle] (m/rotate frame [0 angle 0]))
(defn pitch [frame angle] (m/rotate frame [angle 0 0]))
(defn roll [frame angle] (m/rotate frame [0 0 angle]))

(defn transform? [x]
  #?(:clj (instance? DoubleMat3x4 x)
     :cljs (and (instance? m/Frame x) (= 4 (count (:columns x))))))

(defn rotate
  "Rotate the basis about a global axis, in radians; preserve its translation."
  [frame axis angle]
  #?(:clj (MatrixTransforms/Rotate frame
                                   (case axis
                                     :x (DoubleVec3. 1 0 0)
                                     :y (DoubleVec3. 0 1 0)
                                     :z (DoubleVec3. 0 0 1))
                                   angle)
     :cljs (let [c (js/Math.cos angle)
                 s (js/Math.sin angle)
                 rotate-vector (case axis
                                 :x (fn [[x y z]] [x (- (* c y) (* s z)) (+ (* s y) (* c z))])
                                 :y (fn [[x y z]] [(+ (* c x) (* s z)) y (- (* c z) (* s x))])
                                 :z (fn [[x y z]] [(- (* c x) (* s y)) (+ (* s x) (* c y)) z]))]
             (update frame :columns
                     (fn [columns]
                       (conj (mapv rotate-vector (subvec columns 0 3)) (columns 3)))))))

(defn go-forward
  ([frame distance] (go-forward frame distance :z))
  ([frame distance axis]
   (m/translate frame (case axis
                        :x [distance 0 0]
                        :y [0 distance 0]
                        :z [0 0 distance]))))

(defn go-backward
  ([frame distance] (go-forward frame (- distance)))
  ([frame distance axis] (go-forward frame (- distance) axis)))

(defn translation-vector [frame]
  #?(:clj (let [tr (.getColumn ^DoubleMat3x4 frame 3)]
            [(.x tr) (.y tr) (.z tr)])
     :cljs (get-in frame [:columns 3])))

(defn set-translation
  ([frame [x y z]]
   #?(:clj (DoubleMat3x4. (.getColumn ^DoubleMat3x4 frame 0)
                         (.getColumn ^DoubleMat3x4 frame 1)
                         (.getColumn ^DoubleMat3x4 frame 2)
                         (DoubleVec3. x y z))
      :cljs (assoc-in frame [:columns 3] [x y z])))
  ([frame value axis]
   (set-translation frame (assoc (translation-vector frame) (case axis :x 0 :y 1 :z 2) value))))

(def identity-tf (m/frame 1))

(defn transform [] (m/frame 1))
