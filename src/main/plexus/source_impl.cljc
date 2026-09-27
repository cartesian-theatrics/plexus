(ns plexus.source-impl
  "Shared expansion functions for compiled and SCI source-aware macros.")

(def operations
  '[left right up down curve forward loft hull translate mirror rotate transform
    set set-meta branch frame save-transform offset add-ns result union
    intersection difference insert trim-by-plane to points on-surface off-surface])

(defn located [form file expression]
  (list 'plexus.core/with-source
        (assoc (select-keys (meta form) [:line :column])
               :file file :source-form (pr-str form))
        (list 'clojure.core/fn [] expression)))

(defn operation [op form file args]
  (located form file (cons (symbol "plexus.core" (name op)) args)))

(defn at [form file args]
  (when-not (= 1 (count args))
    (throw (ex-info "plexus.source/at expects one expression" {:form form})))
  (let [expression (first args)
        source (assoc (select-keys (merge (meta form) (meta expression)) [:line :column])
                      :file file :source-form (pr-str expression))]
    (list 'plexus.core/with-source source (list 'clojure.core/fn [] expression))))
