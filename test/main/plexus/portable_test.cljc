(ns plexus.portable-test
  (:require [#?(:clj clojure.test :cljs cljs.test) :refer [deftest is]]
            [clj-manifold3d.core :as m]
            [plexus.core :as p]
            [plexus.transforms :as tf]
            [plexus.utils :as u]))

(defn near? [a b] (< (#?(:clj Math/abs :cljs js/Math.abs) (- a b)) 1.0e-5))
(defn volume [extrusion name] (:volume (m/get-properties (p/get-model extrusion name))))

(deftest ordinary-functions-compose
  (let [built (p/extrude
               (p/result :name :pipe :expr (p/difference :body :mask))
               (p/frame :name :body :cross-section (m/square 10 10 true))
               (p/frame {:name :mask :cross-section (m/square 6 6 true)})
               (map p/forward [{:length 3} {:length 7}]))]
    (is (near? 640 (volume built :pipe)))
    (is (near? 1000 (volume built :body)))
    (is (p/extrusion? built))))

(deftest explicit-deferred-rebuilding
  (let [length (atom 2)
        calls (atom 0)
        built (p/extrude (p/frame :name :body :cross-section (m/square 1 1))
                         (p/deferred #(do (swap! calls inc)
                                          [(p/forward :length @length) nil
                                           [(p/forward :length @length)]])))]
    (is (= 1 @calls))
    (is (near? 4 (volume built :body)))
    (reset! length 3)
    (is (near? 6 (volume (p/rebuild built) :body)))
    (is (= 2 @calls))))

(deftest arguments-evaluated-once
  (let [calls (atom 0)
        segment (p/forward :length (swap! calls inc))]
    (p/extrude (p/frame :name :origin) segment)
    (is (= 1 @calls))))

(deftest points-and-transforms
  (is (= [[0.0 0.0] [0.0 2.0] [0.0 5.0]]
         (p/points :axes [:x :z] (p/frame :name :origin)
                   (p/forward :length 2) (p/forward :length 3))))
  (is (= [0.0 0.0 -3.0] (tf/translation-vector (tf/go-backward (tf/transform) 3 :z))))
  (is (= [1.0 9.0 3.0] (tf/translation-vector (tf/set-translation (tf/set-translation (tf/transform) [1 2 3]) 9 :y))))
  (let [frame (-> (tf/transform) (tf/yaw (/ u/pi 2)) (tf/go-forward 3))]
    (is (every? true? (map near? [3 0 0] (tf/translation-vector frame))))))

#_{:clj-kondo/ignore [:plexus/options]}
(deftest invalid-arguments
  (doseq [f [#(p/forward :length "bad") #(p/forward :length 1 :x 2)
             #(p/frame :cross-section (m/square 1 1)) #(p/branch :with [:a])
             #(p/transform :replace [1 2 3])]]
    (is (try (f) false (catch #?(:clj Exception :cljs :default) _ true)))))

(deftest targeted-nested-and-deferred-forms
  (let [built (p/extrude (p/frame :name :body :cross-section (m/square 2 2))
                         (p/frame :name :other :cross-section (m/square 1 1))
                         (p/deferred (fn [] nil))
                         (p/to :models [:body]
                               (for [_ (range 2)] [(p/forward :length 3)])
                               (p/deferred #(p/forward :length 4)))
                         (p/forward :length 1))]
    (is (near? 44 (volume built :body)))
    (is (near? 1 (volume built :other)))))

(deftest hull-loft-and-insert
  (let [part (p/extrude
               (p/frame :name :body :cross-section (m/square 10 10 true))
               (p/loft :algorithm :eager-nearest-neighbor
                       (p/set :cross-section (m/square 20 20 true))
                       (p/forward :length 10)))
        built (p/extrude (p/frame :name :origin)
                         (p/translate :x 30)
                         (p/insert :extrusion part :models [:body] :ns :part))]
    (is (near? (/ 7000 3) (volume part :body)))
    (is (near? (/ 7000 3) (volume built :part/body))))
  (let [built (p/extrude (p/frame :name :body :cross-section (m/square 2 2))
                         (p/hull (p/forward :length 2)
                                 (p/translate :x 2)
                                 (p/forward :length 2)))]
    (is (near? 24 (volume built :body)))))

(deftest sampled-loft-continues-from-final-transform
  (let [calls (atom [])
        built (p/extrude
               (p/frame :name :rod :cross-section (m/square 1 1))
               (p/loft
                 (p/forward :length 10 :n-steps 4
                            :transform-step-fn (fn [_ i]
                                                 (swap! calls conj i)
                                                 (m/frame [1 0 0] [0 1 0] [0 0 1] [i 0 (* 2.5 i)]))))
               (p/forward :length 2)
               (p/save-transform :frame :rod :name :end))]
    (is (= {0 1 1 1 2 1 3 1 4 1} (frequencies @calls)))
    (is (every? true? (map near? [4 0 12] (tf/translation-vector (p/lookup-transform built :end)))))
    (is (near? 12 (volume built :rod)))))

(deftest curves-branches-and-gap
  (doseq [[op expected] [[p/left [-10 0 10]] [p/right [10 0 10]]
                         [p/up [0 10 10]] [p/down [0 -10 10]]]]
    (let [built (p/extrude (p/frame :name :body :curve-radius 10 :cross-section (m/square 1 1))
                           (p/branch :from :body (p/forward :length 2))
                           (op :angle (/ u/pi 2) :gap true)
                           (p/save-transform :frame :body :name :end))]
      (is (every? true? (map near? expected (tf/translation-vector (p/lookup-transform built :end)))))
      (is (near? 2 (volume built :body))))))

(deftest result-hull-and-threaded-transforms
  (let [built (p/extrude
               (p/result :name :both :expr (->> (p/hull :a :b) (p/translate :x 10)))
               (p/frame :name :a :cross-section (m/square 1 1))
               (p/frame :name :b :cross-section (m/square 1 1))
               (p/translate :x 2 :to [:b])
               (p/forward :length 1))]
    (is (near? 3 (volume built :both)))))

(deftest curve-geometry
  (doseq [op [p/left p/right p/up p/down]]
    (let [built (p/extrude (p/frame :name :body :cross-section (m/square 1 1 true))
                           (op :angle (/ u/pi 2) :curve-radius 10 :cs 64))]
      (is (< 15 (volume built :body) 16))
      (is (= :NoError (m/status (p/get-model built :body)))))))

(def ^:dynamic *length* 2)

(deftest dynamic-bindings-and-captured-values
  (let [make-model (fn [segment]
                     (p/extrude (p/frame :name :body :cross-section (m/square 1 1)) segment))
        captured (make-model (p/forward :length *length*))
        deferred (make-model (p/deferred #(p/forward :length *length*)))]
    (binding [*length* 5]
      (is (near? 2 (volume (p/rebuild captured) :body)))
      (is (near? 5 (volume (p/rebuild deferred) :body))))))

(deftest global-and-local-rotation-conventions
  (let [start (-> (tf/transform) (tf/set-translation [1 2 3]) (tf/yaw (/ u/pi 2)))
        rotated (tf/rotate start :z (/ u/pi 2))]
    (is (every? true? (map near? [1 2 3] (tf/translation-vector rotated))))
    (is (every? true? (map near? [1 5 3] (tf/translation-vector (tf/go-forward rotated 3)))))))

(deftest string-frame-names
  (let [built (p/extrude (p/result :name "result" :expr "body")
                         (p/frame :name "body" :cross-section (m/square 1 1))
                         (p/forward :length 2)
                         (p/save-transform :frame "body" :name "end"))]
    (is (p/frame? (p/get-frame built "body")))
    (is (near? 2 (volume built "result")))
    (is (near? 2 (last (tf/translation-vector (p/lookup-transform built "end")))))))
