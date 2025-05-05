(ns mgit.mgit
  (:require
   [clojure.string :as str]
   [clj-commons.digest :as digest]
   [babashka.fs :as fs])
  (:gen-class))

(declare actions)

(def git-dir (fs/file ".git"))
(def objects-dir (fs/file git-dir "objects"))
(def refs-dir (fs/file git-dir "refs"))

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

(defn cmd-hash-object
  "Compute object ID"
  [& args]
  (let [{:keys [options error]}
        (->> (rest args)
             (parse
              (fn [acc]
                (condp apply [(-> acc :remaining first)]
                  #{"-w"} (-> acc
                              (assoc-in [:options :w] true)
                              (update :remaining next))
                  nil))))]
    (when error
      (throw (ex-info error {:args args})))

    (doseq [filepath (:args options)]
      (let [^bytes content (fs/read-all-bytes filepath)]
        (with-open [byte-out (java.io.ByteArrayOutputStream.)
                    writer (java.io.OutputStreamWriter. byte-out "UTF-8")]
          (.write writer (format "blob %s\0" (count content)))
          (.flush writer)
          (.write byte-out content)
          (println (digest/sha1 (.toByteArray byte-out))))))))

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
              "hash-object" #'cmd-hash-object
              "help" #'cmd-help})

(defn -main [& args]
  (let [action (get actions (first args))]
    (if action
      (apply action args)
      (do
        (println (format "`%s` is not a mgit action.\n" (first args)))
        (cmd-help args)))))
