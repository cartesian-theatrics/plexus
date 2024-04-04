(ns plexus.core_test
  (:require
   [clj-manifold3d.core :as m]
   [clojure.test :as t]
   [plexus.core :as p]))

(defn get-volume [extrusion]
  (-> extrusion
      (p/get-model (:main-model extrusion))
      (m/get-properties)
      (:volume)))

(def default-eps 0.002)

(def pi Math/PI)
(def pi|2 (/ pi 2))
(def pi|2 (/ pi 2))
(def pi|3 (/ pi 3))
(def pi|4 (/ pi 4))
(def pow #(Math/pow %1 %2))

(defn circle-area [r n]
  (let [s (* 2 r (Math/sin (/ (* 2 pi) (* 2 n))))]
    (/ (* n (pow s 2))
       (* 4 (Math/tan (/ pi n))))))

(defn about=
  ([x y] (about= x y default-eps))
  ([x y eps] (< (Math/abs (- x y)) eps)))

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


(t/deftest test-hull)

(t/deftest test-loft)

(t/deftest test-translate)

(t/deftest test-rotate)

(t/deftest test-offset)

(t/deftest test-insert)

(t/deftest test-)
(test-extrudes)
