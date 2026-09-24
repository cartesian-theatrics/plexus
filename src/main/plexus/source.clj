(ns plexus.source
  "Optional source-aware operation macros. Require plexus.core at runtime.
  These evaluate arguments once and delegate to the macro-free core functions."
  (:refer-clojure :exclude [set]))

(defn- caller-file [env]
  ;; The CLJS compiler does not bind clojure.core/*file* at macro call sites.
  ;; Resolve its source var lazily so JVM users do not need ClojureScript installed.
  (if (:ns env)
    (or (:file env)
        (some-> (find-ns 'cljs.analyzer) (ns-resolve '*cljs-file*) deref)
        *file*)
    *file*))

(defn- located [form env expression]
  `(plexus.core/with-source
     ~(assoc (select-keys (meta form) [:line :column])
             :file (caller-file env) :source-form (pr-str form))
     (fn [] ~expression)))

(defmacro at
  "Attach the wrapped expression's source location to its form and errors."
  [form]
  (located (with-meta form (merge (meta &form) (meta form))) &env form))

(defmacro left [& args]
  (located &form &env (cons 'plexus.core/left args)))

(defmacro right [& args]
  (located &form &env (cons 'plexus.core/right args)))

(defmacro up [& args]
  (located &form &env (cons 'plexus.core/up args)))

(defmacro down [& args]
  (located &form &env (cons 'plexus.core/down args)))

(defmacro curve [& args]
  (located &form &env (cons 'plexus.core/curve args)))

(defmacro forward [& args]
  (located &form &env (cons 'plexus.core/forward args)))

(defmacro loft [& args]
  (located &form &env (cons 'plexus.core/loft args)))

(defmacro hull [& args]
  (located &form &env (cons 'plexus.core/hull args)))

(defmacro translate [& args]
  (located &form &env (cons 'plexus.core/translate args)))

(defmacro mirror [& args]
  (located &form &env (cons 'plexus.core/mirror args)))

(defmacro rotate [& args]
  (located &form &env (cons 'plexus.core/rotate args)))

(defmacro transform [& args]
  (located &form &env (cons 'plexus.core/transform args)))

(defmacro set [& args]
  (located &form &env (cons 'plexus.core/set args)))

(defmacro set-meta [& args]
  (located &form &env (cons 'plexus.core/set-meta args)))

(defmacro branch [& args]
  (located &form &env (cons 'plexus.core/branch args)))

(defmacro frame [& args]
  (located &form &env (cons 'plexus.core/frame args)))

(defmacro save-transform [& args]
  (located &form &env (cons 'plexus.core/save-transform args)))

(defmacro offset [& args]
  (located &form &env (cons 'plexus.core/offset args)))

(defmacro add-ns [& args]
  (located &form &env (cons 'plexus.core/add-ns args)))

(defmacro result [& args]
  (located &form &env (cons 'plexus.core/result args)))

(defmacro union [& args]
  (located &form &env (cons 'plexus.core/union args)))

(defmacro intersection [& args]
  (located &form &env (cons 'plexus.core/intersection args)))

(defmacro difference [& args]
  (located &form &env (cons 'plexus.core/difference args)))

(defmacro insert [& args]
  (located &form &env (cons 'plexus.core/insert args)))

(defmacro trim-by-plane [& args]
  (located &form &env (cons 'plexus.core/trim-by-plane args)))

(defmacro to [& args]
  (located &form &env (cons 'plexus.core/to args)))

(defmacro points [& args]
  (located &form &env (cons 'plexus.core/points args)))
