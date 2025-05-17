(ns mgit.mgit
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str])
  (:gen-class))

(declare actions)

(def git-dir (fs/file ".git"))
(def objects-dir (fs/file git-dir "objects"))
(def refs-dir (fs/file git-dir "refs"))

(defn eprintln [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn parse [handler args]
  (reduce
   (fn [acc _]
     (let [arg (-> acc :remaining first)
           more (-> acc :remaining rest)]
       (or
        (when (nil? (:remaining acc))
          (reduced acc))
        (handler acc)
        (when (= arg "--")
          (reduced (-> acc
                       (update-in [:options :args] into more)
                       (assoc :remaining nil))))
        (when (-> (or arg "") (str/starts-with? "-"))
          (reduced {:error (str "Illegal argument: " arg)}))
        (-> acc
            (update-in [:options :args] conj arg)
            (update :remaining next)))))
   {:options {:args []} :remaining args}
   args))

(defn cmd-add
  "Add file contents to the index"
  [& args]
  (let [parsed (->> args
                    (parse
                     (fn [acc]
                       (condp apply [(-> acc :remaining first)]
                         #{"-N"} (-> acc
                                     (assoc-in [:options :N] true)
                                     (update :remaining next))
                         nil))))]
    (eprintln parsed)))

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
    (-> (format "  %s - %s" k (:doc (meta v)))
        eprintln)))

(def actions {"init" #'cmd-init
              "add" #'cmd-add
              "help" #'cmd-help})

(defn -main [& args]
  (let [action (get actions (first args))]
    (if action
      (apply action args)
      (do
        (when (first args)
          (println (format "`%s` is not a mgit action.\n" (first args))))
        (cmd-help args)))))
