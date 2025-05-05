(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'mgit/mgit)
(def main 'mgit.mgit)
(def class-dir "target/classes")
(def uber-file (format "target/%s-standalone.jar" (name lib)))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn uber [_]
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis @basis
                  :ns-compile [main]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main main}))
