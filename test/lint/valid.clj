(ns lint.valid (:require [plexus.core :as p]))
(defn path [distance options]
  [(p/frame :name :body)
   (p/forward :length distance)
   (p/forward :length distance :n-steps nil)
   (p/forward options)
   (p/branch :from :body (p/forward {:z 1}))
   (p/hull (p/forward :x 2))
   (p/hull :a :b :c)
   (p/set-meta :length "metadata")
   (p/result :name :body :expr (p/difference :outer :inner))])

(defn surface-path [surface]
  [(p/on-surface :surface surface :origin [1 2 3] :direction [1 0 0] :depth -1 :step-size 0.5)
   (p/off-surface :to [:body])])
