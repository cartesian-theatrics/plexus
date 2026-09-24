;; Evaluate the real helper definition without loading unrelated top-level
;; models or the scad-etc.utils dependency chain (which contains #dbg readers).
(ns plexus-scad-helper
  (:require [clojure.java.io :as io]
            [plexus.core :as p :refer [left right]]
            [scad-etc.math :refer [pi pi|2 acos tan vec-sub vec-dot vec-magnitude]]))

(def helper
  (binding [*ns* (the-ns 'plexus-scad-helper) *read-eval* false]
    (with-open [reader (java.io.PushbackReader. (io/reader "../scad-etc/src/main/scad_etc/utils.clj"))]
      (loop []
        (let [form (read {:eof ::eof} reader)]
          (cond (= form ::eof) (throw (ex-info "Helper not found" {}))
                (and (seq? form) (= 'defn (first form))
                     (= 'calc-right-forward-left-segment (second form))) (eval form)
                :else (recur)))))))

(doseq [steps [nil 5]
        :let [points (p/points :axes [:x :z] (p/frame :name :origin)
                               (helper 10 40 5 0 steps))]]
  (assert (= (if steps 46 42) (count points)))
  (assert (every? #(< (Math/abs %) 1.0e-9) (map - (last points) [10 40])))
  (prn {:steps steps :count (count points) :last (last points)}))
