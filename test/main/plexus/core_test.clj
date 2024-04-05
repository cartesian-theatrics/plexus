(ns plexus.core-test
  (:require
   [clj-manifold3d.core :as m]
   [clojure.test :as t]
   [plexus.core :as p]))

(def default-eps 0.002)

(defn get-volume [extrusion]
  (-> extrusion
      (p/get-model (:main-model extrusion))
      (m/get-properties)
      (:volume)))

(defn get-props [extrusion]
  (-> extrusion
      (p/get-model (:main-model extrusion))
      (m/get-properties)
      (update :volume double)
      (update :surface-area double)))

(defn about=
  ([x y] (about= x y default-eps))
  ([x y eps] (< (Math/abs (- x y)) eps)))

(defn test-props [props extrusion]
  (let [e-props (get-props extrusion)]
    (doseq [[k x] e-props]
      (let [y (get props k)]
        (t/is (about= x y))))))

(def pi Math/PI)
(def pi|2 (/ pi 2))
(def pi|3 (/ pi 3))
(def pi|4 (/ pi 4))
(def pow #(Math/pow %1 %2))

(defn circle-area [r n]
  (let [s (* 2 r (Math/sin (/ (* 2 pi) (* 2 n))))]
    (/ (* n (pow s 2))
       (* 4 (Math/tan (/ pi n))))))

(t/deftest test-extrudes
  (t/is (about= (get-volume
                 (p/extrude
                  (p/frame :name :body :cross-section (m/square 10 10 true))
                  (p/forward :length 10)))
                1000.0))
  (let [r 10
        cr 100
        cs 200]
    (t/is (about= (get-volume
                   (p/extrude
                    (p/frame :name :body :cross-section (m/circle r cs))
                    (p/left :angle (* 2 pi) :curve-radius cr :cs cs)))
                  (* 2 (pow pi 2) (pow r 2) cr)
                  200))
    (t/is (about=
           3180.828
           (get-volume
            (p/extrude
             (p/result :name :tmp
                       :expr (p/difference :body :mask))
             (p/frame :name :body :cross-section (m/circle 4)
                      :cs 30
                      :curve-radius 30)
             (p/frame :name :mask :cross-section (m/circle 3))
             (for [i (range 4)]
               (p/branch
                :from :body
                (case i
                  0 (p/left :angle pi|2)
                  1 (p/right :angle pi|2)
                  2 (p/up :angle pi|2)
                  3 (p/down :angle pi|2))))))))))

(t/deftest test-export
  (let [extrusion (p/extrude
                   (p/frame :cross-section (m/circle 6) :name :body)
                   (p/forward :length 10))
        vol (get-volume extrusion)]
    (p/export extrusion "test/data/pipes.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2))
    (t/is (= vol (:volume (m/get-properties (m/manifold (m/import-mesh "test/data/pipes.glb"))))))))

(t/deftest test-hull
  (let [m (p/extrude
           (p/result :name :pipes
                     :expr (p/difference :body :mask))

           (p/frame :cross-section (m/circle 6) :name :body)
           (p/frame :cross-section (m/circle 4) :name :mask)
           (p/set :curve-radius 20)
           (p/hull
            (p/hull
             (p/forward :length 20)
             (p/set :cross-section (m/square 20 20 true) :to [:body])
             (p/set :cross-section (m/square 16 16 true) :to [:mask])
             (p/forward :length 20))
            (p/set :cross-section (m/circle 6) :to [:body])
            (p/set :cross-section (m/circle 4) :to [:mask])
            (p/forward :length 20)))]
    (test-props {:surface-area 7264.4707, :volume 7129.058} m)))

(t/deftest test-orientation
  (test-props
   {:surface-area 191659.8125, :volume 143258.34375}
   (p/extrude
    (p/frame :name :body
             :cross-section (-> (m/text "test/data/Cinzel-VariableFont_wght.ttf" "abc" 10 5 :non-zero)
                                (m/scale-to-height 20)
                                (m/center))
             :curve-radius 50)
    (for [i (range 4)]
      (p/branch
       :from :body
       (case i
         0 [(p/left :angle pi|2)
            (p/right :angle pi|2)]
         1 [(p/right :angle pi|2)
            (p/left :angle pi|2)]
         2 [(p/up :angle pi|2)
            (p/down :angle pi|2)]
         3 [(p/down :angle pi|2)
            (p/up :angle pi|2)]))))))
