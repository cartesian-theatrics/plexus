(ns hooks.plexus
  (:require [clj-kondo.hooks-api :as api]))

(def required
  {'frame #{:name} 'branch #{:from} 'result #{:name :expr}
   'insert #{:extrusion} 'transform #{:replace} 'mirror #{:normal}
   'offset #{:delta} 'add-ns #{:namespace} 'save-transform #{:frame}
   'to #{:models}})

(def numeric-options
  #{:x :y :z :length :angle :arc-length :curve-radius :roll :delta :simplify
    :origin-offset :twist})

(defn finding! [node message]
  (api/reg-finding! (assoc (meta node) :type :plexus/options :message message)))

(defn literal? [node]
  (let [value (api/sexpr node)]
    (or (nil? value) (number? value) (string? value) (keyword? value)
        (boolean? value)
        (and (#{:vector :map :set} (api/tag node))
             (every? literal? (:children node))))))

(defn option-pairs [nodes allowed]
  (loop [[k v & more :as nodes] nodes pairs []]
    (cond
      (empty? nodes) pairs
      (and (api/keyword-node? k) (or (nil? allowed) (allowed (api/sexpr k))))
      (if v
        (recur more (conj pairs [k v]))
        (do (finding! k "Missing option value") pairs))
      :else pairs)))

(defn operation [{:keys [node]}]
  (let [[callee & args] (:children node)
        op (:name (api/resolve {:name (api/sexpr callee) :call true}))
        first-arg (first args)
        ;; A computed options map cannot be checked without executing user code.
        known? (or (empty? args) (api/keyword-node? first-arg) (api/map-node? first-arg))
        pairs (if (api/map-node? first-arg)
                (partition 2 (:children first-arg))
                (option-pairs args (when (#{'hull 'loft} op) #{:to :algorithm})))
        options (into {} (map (fn [[k v]] [(api/sexpr k) v]) pairs))]
    (when known?
      (doseq [k (get required op) :when (not (contains? options k))]
        (finding! node (str "Missing required option " k)))
      (when (= op 'forward)
        (when (not= 1 (count (filter #(contains? options %) [:length :x :y :z])))
          (finding! node "forward requires exactly one of :length, :x, :y, :z")))
      (when (and (= op 'translate) (not-any? #(contains? options %) [:x :y :z]))
        (finding! node "translate requires :x, :y, or :z")))
    (doseq [[k value-node] options :when (literal? value-node)]
      (let [v (api/sexpr value-node)
            valid? (cond
                     ;; set and set-meta deliberately accept arbitrary defaults/metadata.
                     (#{'set 'set-meta} op) true
                     (numeric-options k) (number? v)
                     (= :n-steps k) (pos-int? v)
                     (= :cs k) (int? v)
                     (#{:name :from :namespace :frame} k) (or (keyword? v) (string? v))
                     (= :normal k) (and (vector? v) (= 3 (count v)) (every? number? v))
                     (= :global? k) (boolean? v)
                     (= :direction k) (contains? #{:left :right :up :down} v)
                     (= :join-type k) (contains? #{:square :round :miter} v)
                     (= :transform-step-fn k) false
                     :else true)]
        (when-not valid?
          (finding! value-node (str "Invalid literal value for " k)))))
    ;; Leave the original call intact: normal symbol/arity analysis still applies.
    nil))
