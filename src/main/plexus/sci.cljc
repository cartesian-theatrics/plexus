(ns plexus.sci
  "Optional SCI integration. Add org.babashka/sci to the host's dependencies.
  Compile this namespace into the host; interpreted models call ordinary functions."
  (:require [sci.core :as sci]
            [plexus.core]
            [plexus.transforms]
            [plexus.utils]))

(def namespaces
  {'plexus.core #_{:clj-kondo/ignore [:unresolved-symbol]}
   (sci/copy-ns plexus.core (sci/create-ns 'plexus.core)
                             {:exclude [defmodel export-models]})
   'plexus.transforms (sci/copy-ns plexus.transforms (sci/create-ns 'plexus.transforms))
   'plexus.utils (sci/copy-ns plexus.utils (sci/create-ns 'plexus.utils))})

(def config {:namespaces namespaces})
