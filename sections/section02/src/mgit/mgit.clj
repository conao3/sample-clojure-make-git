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

(defn parse-index [^bytes data]
  (let [bytes->int #(reduce (fn [acc elm] (+ (bit-shift-left acc 8) (bit-and elm 0xff))) 0 %)
        bytes->str #(-> % byte-array String.)
        bytes->hexstr #(->> % ((partial map (partial format "%02x"))) (apply str))
        parse (fn [acc key parse-fn remaining-update-fn]
                (-> acc
                    (assoc-in [:parsed key] (parse-fn acc))
                    (update :remaining remaining-update-fn)))
        parse-bytes (fn [acc key size & [update-value-fn]]
                      (-> acc
                          (parse key #(take size (:remaining %)) (partial drop size))
                          (update-in [:parsed key] (or update-value-fn identity))))
        parse-int (fn [acc key size]
                    (-> acc
                        (parse-bytes key size bytes->int)))
        parse-uint16 (fn [acc key] (parse-int acc key 2))
        parse-uint32 (fn [acc key] (parse-int acc key 4))
        parse-dirc (fn [acc key]
                     (parse acc
                            key
                            #(do
                               (when-not (= (take 4 (:remaining %))
                                            (seq (. "DIRC" getBytes)))
                                 (throw (ex-info "Data should be start with `DIRC'" {:acc %})))
                               true)
                            (partial drop 4)))
        parse-filepath (fn [acc key]
                         (let [res (loop [bytes []
                                          find-null false
                                          remaining (:remaining acc)]
                                     (let [d (first remaining)]
                                       (cond
                                         (= d 0)
                                         (recur bytes true (rest remaining))

                                         (and (not= d 0) (not find-null))
                                         (recur (conj bytes d) false (rest remaining))

                                         (and (not= d 0) find-null)
                                         {:bytes bytes :remaining remaining})))]
                           (-> acc
                               (assoc-in [:parsed key] (bytes->str (:bytes res)))
                               (assoc :remaining (:remaining res)))))
        parse-entry (fn [acc key]
                      (let [res (-> (assoc acc :parsed {})
                                    (parse-uint32 :ctime-sec)
                                    (parse-uint32 :ctime-nsec)
                                    (parse-uint32 :mtime-sec)
                                    (parse-uint32 :mtime-nsec)
                                    (parse-uint32 :dev)
                                    (parse-uint32 :ino)
                                    (parse-uint32 :mode)
                                    (parse-uint32 :uid)
                                    (parse-uint32 :gid)
                                    (parse-uint32 :size)
                                    (parse-bytes :object-id 20 bytes->hexstr)
                                    (parse-uint16 :flag)
                                    (parse-filepath :filepath))]
                        (-> acc
                            (update-in [:parsed key] #(or % []))
                            (update-in [:parsed key] conj (:parsed res))
                            (assoc :remaining (:remaining res)))))
        parse-entries (fn [acc key]
                        (nth (iterate #(parse-entry % key) acc)
                             (-> acc :parsed :number-of-entries)))]
    (-> {:parsed {} :remaining (seq data)}
        (parse-dirc :dirc)
        (parse-uint32 :version)
        (parse-uint32 :number-of-entries)
        (parse-entries :entries)
        (parse-bytes :checksum 20 bytes->hexstr))))

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

(defn cmd-init
  "Initialize git repository"
  [& _args]
  (doseq [dir [git-dir objects-dir refs-dir]]
    (-> dir fs/create-dirs))

  (->> "ref: refs/heads/master\n"
       (spit (fs/file git-dir "HEAD")))

  (-> (format "Initialized empty Git repository in %s" (-> git-dir fs/absolutize))
      eprintln))

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

(require 'clojure.pprint)

(defn cmd-ls-files
  "Parse .git/index"
  [& _args]
  (-> (fs/file git-dir "index")
      fs/read-all-bytes
      parse-index
      clojure.pprint/pprint))

(defn cmd-help
  "Help"
  [& _args]
  (println "Available actions:")
  (doseq [[k v] actions]
    (-> (format "  %s - %s" k (:doc (meta v)))
        eprintln)))

(def actions {"init" #'cmd-init
              "add" #'cmd-add
              "ls-files" #'cmd-ls-files
              "help" #'cmd-help})

(defn -main [& args]
  (let [action (get actions (first args))]
    (if action
      (apply action args)
      (do
        (when (first args)
          (println (format "`%s` is not a mgit action.\n" (first args))))
        (cmd-help args)))))
