(ns plexus.test-runner
  (:require [cljs.test :as t :refer-macros [run-tests]]
            [clj-manifold3d.core :as m]
            [plexus.portable-test]
            [plexus.sci-test]
            [goog.object :as gobj]))

(defmethod t/report [::t/default :end-run-tests] [result]
  (gobj/set js/process "exitCode" (if (and (pos? (:test result)) (t/successful? result)) 0 1)))

(defn main []
  (let [resolve (gobj/get (js/require "node:path") "resolve")
        factory (js/require (resolve "../clj-manifold3d/public/wasm/manifold.js"))
        bytes ((gobj/get (js/require "node:fs") "readFileSync")
               (resolve "../clj-manifold3d/public/wasm/manifold.wasm"))]
    (-> (m/init! {:factory factory :wasm-binary bytes})
        (.then (fn [_] (m/with-disposal #(run-tests 'plexus.portable-test 'plexus.sci-test))))
        (.catch (fn [e] (js/console.error e) (gobj/set js/process "exitCode" 1))))))
