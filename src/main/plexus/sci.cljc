(ns plexus.sci
  "Optional SCI integration. Add org.babashka/sci to the host's dependencies.
  Compile this namespace into the host for the function API and source-aware macro DSL."
  (:require [sci.core :as sci]
            [plexus.core]
            [plexus.source-impl :as source]
            [plexus.transforms]
            [plexus.utils]))

(def source-macros
  (let [ns (sci/create-ns 'plexus.source)]
    (into {'at (sci/new-macro-var
                 'at (fn [form _env & args] (source/at form @sci/file args))
                 {:ns ns :doc "Attach an expression's source location to its form and errors."
                  :arglists '([form])})}
          (map (fn [op]
                 [op (sci/new-macro-var
                       op (fn [form _env & args]
                            (source/operation op form @sci/file args))
                       {:ns ns :doc (str "Source-aware macro for plexus.core/" op ".")
                        :arglists '([& opts])})])
               source/operations))))

(def namespaces
  {'plexus.source source-macros
   'plexus.core #_{:clj-kondo/ignore [:unresolved-symbol]}
   (sci/copy-ns plexus.core (sci/create-ns 'plexus.core)
                             {:exclude [defmodel export-models]})
   'plexus.transforms (sci/copy-ns plexus.transforms (sci/create-ns 'plexus.transforms))
   'plexus.utils (sci/copy-ns plexus.utils (sci/create-ns 'plexus.utils))})

(def config {:namespaces namespaces})
