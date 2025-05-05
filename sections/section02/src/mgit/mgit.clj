(ns mgit.mgit
  (:require
   [clojure.string :as str]
   [babashka.fs :as fs])
  (:gen-class))

(declare actions)

(def git-dir (fs/file ".git"))
(def objects-dir (fs/file git-dir "objects"))
(def refs-dir (fs/file git-dir "refs"))

(defn cmd-hash-object
  "Compute object ID"
  [& args]
  (let [parsed (reduce (fn [acc _]
                         (if (nil? (:remaining acc))
                           (reduced acc)
                           (let [arg (-> acc :remaining first)
                                 more (-> acc :remaining rest)]
                             (condp apply [arg]
                               #{"-w"} (-> acc
                                           (assoc-in [:options :w] true)
                                           (update :remaining next))
                               #{"--"} (reduced (-> acc
                                                    (update-in [:options :args] into more)
                                                    (assoc :remaining nil)))
                               #(-> (or % "") (str/starts-with? "-"))
                               (reduced {:error (str "Illegal argument: " arg)})
                               (-> acc
                                   (update-in [:options :args] conj arg)
                                   (update :remaining next))))))
                       {:options {:args []} :remaining args}
                       args)]
    parsed))

(defn cmd-init
  "Initialize git repository"
  [& _args]
  (doseq [dir [git-dir objects-dir refs-dir]]
    (fs/create-dirs dir))

  (doseq [[path contents] {"config" "[core]\n\trepositoryformatversion = 0\n\tfilemode = true\n\tbare = false\n\tlogallrefupdates = true\n"
                           "HEAD" "ref: refs/heads/master\n"}]
    (->> contents
         (spit (fs/file git-dir path))))

  (println (format "Initialized empty Git repository in %s" (fs/absolutize git-dir))))

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
        (println (format "`%s` is not a mgit action.\n" (first args)))
        (cmd-help args)))))
