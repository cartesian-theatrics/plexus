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

(deftest interpreted-explicit-source-location
  (let [result
        (sci/eval-string* (sci/init ps/config)
          (str "(require '[plexus.core :as p])
                (try
                  (p/extrude (p/frame :name :origin)
                    (p/with-source {:file \"editor.clj\" :line 42 :column 7}
                      (fn [] (p/left :angle 1))))
                  (catch " #?(:clj "Exception" :cljs ":default") " e
                    (select-keys (ex-data e) [:file :line :column :key])))"))]
    (is (= {:file "editor.clj" :line 42 :column 7 :key :plexus.impl/left} result))))

(defn eval-file [ctx file code]
  (sci/binding [sci/file file]
    (sci/eval-string* ctx code)))

(defn capture-code [expression]
  (str "(try\n  " expression "\n  (catch " #?(:clj "Exception" :cljs ":default") " e (ex-data e)))"))

(deftest interpreted-macro-model-and-rebuild
  (let [ctx (sci/init (assoc-in ps/config [:namespaces 'clj-manifold3d.core]
                               {'square m/square 'get-properties m/get-properties}))
        result (eval-file ctx "pipe.clj"
                 "(require '[plexus.core :as p] '[plexus.source :as s]
                           '[clj-manifold3d.core :as m])
                  (def ^:dynamic *length* 2)
                  (def calls (atom 0))
                  (def model
                    (p/extrude
                      (s/result :name :pipe :expr (s/difference :body :mask))
                      (s/frame :name :body :cross-section (m/square 10 10 true))
                      (s/frame :name :mask :cross-section (m/square 6 6 true))
                      (s/branch :from :body
                        (s/at (p/deferred
                          (fn [] (for [i (range 3)]
                            (s/forward {:length (do (swap! calls inc) *length*)}))))))))
                  (def rebuilt (binding [*length* 3] (p/rebuild model)))
                  [(:volume (m/get-properties (p/get-model model :pipe)))
                   (:volume (m/get-properties (p/get-model rebuilt :pipe)))
                   @calls]")]
    (is (< (#?(:clj Math/abs :cljs js/Math.abs) (- 384 (first result))) 1.0e-5))
    (is (< (#?(:clj Math/abs :cljs js/Math.abs) (- 576 (second result))) 1.0e-5))
    (is (= 6 (last result)))))

(deftest interpreted-macro-location-survives-later-evaluation
  (let [ctx (sci/init ps/config)
        source (eval-file ctx "segments.clj"
                 "(require '[plexus.source :as s] '[plexus.core :as p])
(def stored (s/left :angle 1))
(meta stored)")
        error (eval-file ctx "build.clj"
                (capture-code "(p/extrude (s/frame :name :origin) (s/branch :from :origin [stored]))"))]
    (is (= {:file "segments.clj" :line 2 :column 13 :source-form "(s/left :angle 1)"} source))
    (is (= source (select-keys error [:file :line :column :source-form])))
    (is (= :plexus.impl/left (:key error)))
    (is (= {:op :plexus.impl/left :angle 1} (:form error)))))

(deftest interpreted-macro-constructor-errors
  (let [ctx (sci/init ps/config)
        _ (eval-file ctx "setup.clj" "(require '[plexus.source :as s])")
        error (eval-file ctx "invalid.clj" (capture-code "(s/forward :length \"bad\")"))]
    (is (= {:file "invalid.clj" :line 2 :column 3 :source-form "(s/forward :length \"bad\")"}
           (select-keys error [:file :line :column :source-form])))
    (is (= :plexus.impl/forward (:key error)))
    (is (seq (:errors error)))))

(deftest interpreted-macro-result-expression-errors
  (let [ctx (sci/init ps/config)
        source (eval-file ctx "results.clj"
                 "(require '[plexus.source :as s] '[plexus.core :as p])
(def expr (s/union :missing :body))
(meta expr)")
        error (eval-file ctx "build.clj"
                (capture-code "(p/extrude (s/result :name :broken :expr expr) (s/frame :name :body))"))]
    (is (= source (select-keys error [:file :line :column :source-form])))
    (is (= :missing (:form error)))))

(deftest interpreted-at-preserves-wrapped-source
  (let [ctx (sci/init ps/config)
        source (eval-file ctx "wrapped.clj"
                 "(require '[plexus.source :as s] '[plexus.core :as p])
(meta (s/at (p/forward :length 1)))")]
    (is (= {:file "wrapped.clj" :line 2 :column 13 :source-form "(p/forward :length 1)"} source))))

(deftest interpreted-deferred-macro-location-survives-rebuild
  (let [ctx (sci/init ps/config)
        _ (eval-file ctx "deferred.clj"
            "(require '[plexus.source :as s] '[plexus.core :as p])
(def fail? (atom false))
(def segment
  (s/at (p/deferred (fn []
    (if @fail? (s/left :angle 1) (s/forward :length 1))))))
(def built (p/extrude (s/frame :name :origin) segment))")
        _ (eval-file ctx "rebuild.clj" "(reset! fail? true)")
        error (eval-file ctx "rebuild.clj" (capture-code "(p/rebuild built)"))]
    (is (= {:file "deferred.clj" :line 5 :column 16 :source-form "(s/left :angle 1)"}
           (select-keys error [:file :line :column :source-form])))
    (is (= :plexus.impl/left (:key error)))))

(deftest interpreted-surface-navigation-and-source-errors
  (let [ctx (sci/init (assoc-in ps/config [:namespaces 'clj-manifold3d.core]
                               {'cube m/cube 'square m/square 'get-properties m/get-properties}))
        result (eval-file ctx "surface.clj"
                 "(require '[plexus.core :as p] '[plexus.source :as s]
                           '[clj-manifold3d.core :as m])
                  (def surface (p/surface (m/cube 10 10 10)))
                  (def model
                    (p/extrude
                      (s/frame :name :path :cross-section (m/square 1 1 true))
                      (s/on-surface :surface surface :origin [2 3 10] :direction [1 0 0])
                      (s/forward :length 2)
                      (s/off-surface)))
                  (:volume (m/get-properties (p/get-model model :path)))")
        error (eval-file ctx "surface-error.clj"
                (capture-code
                  "(p/extrude (s/frame :name :path)
                     (s/on-surface :surface surface :origin [9 9 10] :direction [1 1 0])
                     (s/forward :length 4))"))]
    (is (< (#?(:clj Math/abs :cljs js/Math.abs) (- result 2)) 1e-5))
    (is (= :vertex-singularity (:reason error)))
    (is (= :plexus.impl/forward (:key error)))
    (is (= "surface-error.clj" (:file error)))
    (is (= "(s/forward :length 4)" (:source-form error)))))
