(ns plexus.forward-sampling-test
  (:require [clojure.test :refer [deftest is]]
            [plexus.core :as p]
            [clj-manifold3d.core :as m]))

(deftest loft-retains-the-entering-cross-section
  (let [model (p/get-model
               (p/extrude (p/frame :name :taper :cross-section (m/square 10 10 true))
                          (p/loft (p/set :cross-section (m/square 20 20 true))
                                  (p/forward :length 10))) :taper)
        at-height (fn [z] (filter #(< (Math/abs (- (nth % 2) z)) 1.0e-6) (m/get-vertices model)))]
    (is (= :NoError (m/status model)))
    (is (= 5.0 (double (apply max (map first (at-height 0))))))
    (is (= 10.0 (double (apply max (map first (at-height 10))))))
    (is (< (Math/abs (- (:volume (m/get-properties model)) (/ 7000.0 3))) 1.0e-5))))

(deftest custom-path-includes-final-frame-and-continues-from-it
  (let [calls (atom [])
        built (p/extrude
               (p/frame :name :rod :cross-section (m/circle 1 32))
               (p/loft (p/forward :length 10 :n-steps 4
                                  :transform-step-fn
                                  (fn [_ i]
                                    (swap! calls conj i)
                                    (m/frame [1 0 0] [0 1 0] [0 0 1] [i 0 (* 2.5 i)]))))
               (p/forward :length 2))
        model (p/get-model built :rod)
        probe (m/translate (m/cube [0.5 0.5 0.5]) [3.75 -0.25 10.75])]
    (is (= {0 1 1 1 2 1 3 1 4 1} (frequencies @calls)))
    (is (= :NoError (m/status model)))
    (is (< (Math/abs (- 0.125 (:volume (m/get-properties (m/intersection model probe))))) 1.0e-6))
    (is (= 1 (count (m/decompose model))))))

(deftest sample-count-does-not-depend-on-floating-point-quotient
  (let [calls (atom [])]
    (p/extrude
     (p/frame :name :rod :cross-section (m/circle 1 32))
     (p/forward :length 0.3 :n-steps 7
                :transform-step-fn (fn [tf i] (swap! calls conj i) tf)))
    (is (= (set (range 8)) (set @calls)))
    (is (= 8 (count @calls)))))
