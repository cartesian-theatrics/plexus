(ns deploy
  (:require
   [badigeon.exec :as exec]
   [badigeon.sign :as sign]
   [badigeon.deploy :as deploy]
   [badigeon.jar :as jar]
   [clojure.string :as s]
   [clojure.tools.deps.alpha :as deps]
   [clojure.java.io :as io]))

(defn get-version-tag []
  (let [version (s/trim (with-out-str
                          (exec/exec
                           "git"
                           {:proc-args ["describe" "--tags" "--exact-match"]
                            ;; The error message of the exception thrown upon error.
                            :error-msg "Failed to get tags"})))]
    (assert (re-find #"\d+\.\d+\.\d+$" version) (format "Version ill-formated: %s" version))
    version))

(defn build-jar [version]
  (assert (re-matches #"\d+\.\d+\.\d+" version) "Expected a release version")
  (let [deps-data (deps/slurp-deps (io/file "deps.edn"))
        deps (:deps deps-data)]
    (jar/jar 'org.clojars.cartesiantheatrics/plexus {:mvn/version version}
             {:out-path                (format "target/plexus-%s.jar" version)
              :paths                   ["src/main" "resources"]
              :deps                    deps
              :mvn/repos               '{"clojars" {:url "https://repo.clojars.org/"}}
              :exclusion-predicate     jar/default-exclusion-predicate})))

(defn deploy-lib [version]
  (let [artifacts (-> [{:file-path (format "target/plexus-%s.jar" version)
                        :extension "jar"}
                       {:file-path "pom.xml"
                        :extension "pom"}]
                      (badigeon.sign/sign {:command "gpg"}))]
    (deploy/deploy 'org.clojars.cartesiantheatrics/plexus
                   version
                   artifacts
                   {:url "https://repo.clojars.org/"
                    :id "clojars"})))

(defn -main [& args]
  (let [version (get-version-tag)]
    (build-jar version)
    (deploy-lib version)))
