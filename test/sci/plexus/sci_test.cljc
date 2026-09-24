(ns plexus.sci-test
  (:require [#?(:clj clojure.test :cljs cljs.test) :refer [deftest is]]
            [sci.core :as sci]
            [plexus.sci :as ps]
            [clj-manifold3d.core :as m]))

(deftest interpreted-model
  (let [ctx (sci/init (assoc-in ps/config [:namespaces 'clj-manifold3d.core]
                               {'square m/square 'get-properties m/get-properties}))
        result (sci/eval-string* ctx
                  "(require '[plexus.core :as p] '[clj-manifold3d.core :as m])
                   (def length (atom 2))
                   (def model
                     (p/extrude
                       (p/result :name :pipe :expr (p/difference :body :mask))
                       (p/frame :name :body :cross-section (m/square 10 10 true))
                       (p/frame :name :mask :cross-section (m/square 6 6 true))
                       (p/deferred (fn [] (for [i (range 3)] (p/forward :length @length))))))
                   (reset! length 3)
                   (:volume (m/get-properties (p/get-model (p/rebuild model) :pipe)))")]
    (is (< (#?(:clj Math/abs :cljs js/Math.abs) (- 576 result)) 1.0e-5))))

(deftest interpreted-dynamic-binding
  (is (= [[0.0 0.0] [0.0 7.0]]
         (sci/eval-string* (sci/init ps/config)
           "(require '[plexus.core :as p])
            (def ^:dynamic *length* 2)
            (def segment (p/deferred (fn [] (p/forward :length *length*))))
            (binding [*length* 7]
              (p/points :axes [:x :z] (p/frame :name :origin) segment))"))))
