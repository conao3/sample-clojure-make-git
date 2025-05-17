(ns mgit.mgit
  (:require [babashka.fs :as fs])
  (:gen-class))

(declare actions)

(def git-dir (fs/file ".git"))
(def objects-dir (fs/file git-dir "objects"))
(def refs-dir (fs/file git-dir "refs"))

(defn eprintln [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn cmd-init
  "Initialize git repository"
  [& _args]
  (doseq [dir [git-dir objects-dir refs-dir]]
    (-> dir fs/create-dirs))

  (->> "ref: refs/heads/master\n"
       (spit (fs/file git-dir "HEAD")))

  (-> (format "Initialized empty Git repository in %s" (-> git-dir fs/absolutize))
      eprintln))

(defn cmd-help
  "Help"
  [& _args]
  (println "Available actions:")
  (doseq [[k v] actions]
    (println (format "  %s - %s" k (:doc (meta v))))))

(def actions {"init" #'cmd-init
              "help" #'cmd-help})

(defn -main [& args]
  (let [action (get actions (first args))]
    (if action
      (apply action args)
      (do
        (when (first args)
          (println (format "`%s` is not a mgit action.\n" (first args))))
        (cmd-help args)))))
