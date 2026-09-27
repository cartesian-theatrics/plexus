(ns plexus.source
  "Optional source-aware operation macros. Require plexus.core at runtime.
  These evaluate arguments once and delegate to the macro-free core functions."
  (:refer-clojure :exclude [set])
  (:require [plexus.source-impl :as source]))

(defn- caller-file [env]
  ;; The CLJS compiler does not bind clojure.core/*file* at macro call sites.
  ;; Resolve its source var lazily so JVM users do not need ClojureScript installed.
  (if (:ns env)
    (or (:file env)
        (some-> (find-ns 'cljs.analyzer) (ns-resolve '*cljs-file*) deref)
        *file*)
    *file*))

(defmacro at
  "Attach the wrapped expression's source location to its form and errors."
  [form]
  (source/at &form (caller-file &env) [form]))

(defmacro left [& args]
  (source/operation 'left &form (caller-file &env) args))

(defmacro right [& args]
  (source/operation 'right &form (caller-file &env) args))

(defmacro up [& args]
  (source/operation 'up &form (caller-file &env) args))

(defmacro down [& args]
  (source/operation 'down &form (caller-file &env) args))

(defmacro curve [& args]
  (source/operation 'curve &form (caller-file &env) args))

(defmacro forward [& args]
  (source/operation 'forward &form (caller-file &env) args))

(defmacro loft [& args]
  (source/operation 'loft &form (caller-file &env) args))

(defmacro hull [& args]
  (source/operation 'hull &form (caller-file &env) args))

(defmacro translate [& args]
  (source/operation 'translate &form (caller-file &env) args))

(defmacro mirror [& args]
  (source/operation 'mirror &form (caller-file &env) args))

(defmacro rotate [& args]
  (source/operation 'rotate &form (caller-file &env) args))

(defmacro transform [& args]
  (source/operation 'transform &form (caller-file &env) args))

(defmacro set [& args]
  (source/operation 'set &form (caller-file &env) args))

(defmacro set-meta [& args]
  (source/operation 'set-meta &form (caller-file &env) args))

(defmacro branch [& args]
  (source/operation 'branch &form (caller-file &env) args))

(defmacro frame [& args]
  (source/operation 'frame &form (caller-file &env) args))

(defmacro save-transform [& args]
  (source/operation 'save-transform &form (caller-file &env) args))

(defmacro offset [& args]
  (source/operation 'offset &form (caller-file &env) args))

(defmacro add-ns [& args]
  (source/operation 'add-ns &form (caller-file &env) args))

(defmacro result [& args]
  (source/operation 'result &form (caller-file &env) args))

(defmacro union [& args]
  (source/operation 'union &form (caller-file &env) args))

(defmacro intersection [& args]
  (source/operation 'intersection &form (caller-file &env) args))

(defmacro difference [& args]
  (source/operation 'difference &form (caller-file &env) args))

(defmacro insert [& args]
  (source/operation 'insert &form (caller-file &env) args))

(defmacro trim-by-plane [& args]
  (source/operation 'trim-by-plane &form (caller-file &env) args))

(defmacro to [& args]
  (source/operation 'to &form (caller-file &env) args))

(defmacro points [& args]
  (source/operation 'points &form (caller-file &env) args))

(defmacro on-surface [& args]
  (source/operation 'on-surface &form (caller-file &env) args))

(defmacro off-surface [& args]
  (source/operation 'off-surface &form (caller-file &env) args))
