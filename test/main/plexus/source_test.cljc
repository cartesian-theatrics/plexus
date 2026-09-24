(ns plexus.source-test
  (:require [#?(:clj clojure.test :cljs cljs.test) :refer [deftest is]]
            [clojure.string :as str]
            [plexus.core :as p]
            #?(:clj [plexus.source :as src]))
  #?(:cljs (:require-macros [plexus.source :as src])))

(defn caught [f]
  (try (f) nil (catch #?(:clj Exception :cljs :default) error error)))

(defn location [error]
  (select-keys (ex-data error) [:file :line :column :source-form]))

(deftest operation-source-survives-nesting
  (let [bad (src/left :angle 1)
        error (caught #(p/extrude (src/frame :name :origin)
                                  (src/branch :from :origin [bad])))
        data (ex-data error)]
    (is (= :plexus.impl/left (:key data)))
    (is (= bad (:form data)))
    (is (= (meta bad) (location error)))
    (is (pos-int? (:line data)))
    (is (pos-int? (:column data)))
    (is (str/ends-with? (:file data) "plexus/source_test.cljc"))
    (is (= "(src/left :angle 1)" (:source-form data)))
    (is (some? (ex-cause error)))))

(deftest constructor-errors-have-source
  (let [error (caught #_ {:clj-kondo/ignore [:plexus/options]}
                      #(src/forward :length "bad"))
        data (ex-data error)]
    (is (= :plexus.impl/forward (:key data)))
    (is (= :plexus.impl/forward (first (:form data))))
    (is (= "(src/forward :length \"bad\")" (:source-form data)))
    (is (pos-int? (:line data)))))

(deftest explicit-source-without-macros
  (let [source {:file "editor.clj" :line 42 :column 7 :source-form "(left :angle a)"}
        bad (p/with-source source #(p/left :angle 1))
        error (caught #(p/extrude (p/frame :name :origin) bad))]
    (is (= source (location error)))
    (is (= bad (:form (ex-data error))))))

(deftest deferred-source-is-not-the-previous-operation
  (let [source {:file "deferred.clj" :line 12 :column 3}
        bad (p/with-source source #(p/deferred (fn [] (throw (ex-info "builder failed" {})))))
        error (caught #(p/extrude (src/frame :name :origin) bad))]
    (is (= source (location error)))
    (is (= bad (:form (ex-data error))))))

(deftest result-expression-keeps-innermost-source
  (let [expr (src/union :missing :body)
        error (caught #(p/extrude (src/result :name :broken :expr expr)
                                  (p/frame :name :body)))]
    (is (= (meta expr) (location error)))
    (is (= :missing (:form (ex-data error))))))

(deftest source-aware-arguments-are-evaluated-once
  (let [calls (atom 0)
        form (src/at (p/forward :length (swap! calls inc)))]
    (p/extrude (p/frame :name :origin) form)
    (is (= 1 @calls))
    (is (pos-int? (:line (meta form))))))

(deftest rebuild-retains-the-deferred-form-location
  (let [fail? (atom false)
        form (src/at (p/deferred #(if @fail?
                                   (p/left :angle 1)
                                   (p/forward :length 1))))
        built (p/extrude (p/frame :name :origin) form)]
    (reset! fail? true)
    (let [error (caught #(p/rebuild built))]
      (is (= (meta form) (location error)))
      (is (= :plexus.impl/left (:key (ex-data error)))))))
