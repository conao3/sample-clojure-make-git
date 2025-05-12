(ns mgit.mgit
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [clj-commons.digest :as digest]
   [babashka.fs :as fs])
  (:import
   [java.util.zip Deflater Inflater]
   [java.io ByteArrayOutputStream OutputStreamWriter])
  (:gen-class))

(declare actions)

(def git-dir (fs/file ".git"))
(def objects-dir (fs/file git-dir "objects"))
(def refs-dir (fs/file git-dir "refs"))

(defn zlib-compress ^bytes [^bytes data]
  (let [deflater (doto (Deflater.)
                   (.setInput data)
                   (.finish))
        buffer (byte-array 1024)]
    (with-open [byte-stream (ByteArrayOutputStream.)]
      (loop []
        (let [size (. deflater deflate buffer)]
          (when (pos? size)
            (. byte-stream write buffer 0 size)
            (recur))))
      (. deflater end)
      (. byte-stream toByteArray))))

(defn zlib-decompress ^bytes [^bytes data]
  (let [inflater (doto (Inflater.)
                   (.setInput data))
        buffer (byte-array 1024)]
    (with-open [byte-stream (ByteArrayOutputStream.)]
      (loop []
        (let [size (. inflater inflate buffer)]
          (when (pos? size)
            (. byte-stream write buffer 0 size)
            (recur))))
      (. inflater end)
      (. byte-stream toByteArray))))

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
      (let [^bytes content (fs/read-all-bytes filepath)
            ^bytes blob (with-open [byte-stream (ByteArrayOutputStream.)
                                    writer (OutputStreamWriter. byte-stream "UTF-8")]
                          (. writer write (format "blob %s\0" (count content)))
                          (. writer flush)
                          (. byte-stream write content)
                          (. byte-stream toByteArray))
            hash (digest/sha1 blob)]
        (println hash)
        (when (:w options)
          (let [dir-name (subs hash 0 2)
                file-name (subs hash 2)
                file-path (fs/file objects-dir dir-name file-name)]
            (fs/create-dirs (fs/parent file-path))
            (fs/write-bytes file-path (zlib-compress blob))))))))

(defn cmd-cat-file
  "Output the contents of git object"
  [& args]
  (let [{:keys [options error]}
        (->> (rest args)
             (parse
              (fn [acc]
                (condp apply [(-> acc :remaining first)]
                  #{"-p"} (-> acc
                              (assoc-in [:options :p] true)
                              (update :remaining next))
                  nil))))]
    (when error
      (throw (ex-info error {:args args})))

    (when (not= 1 (count (:args options)))
      (throw (ex-info "fatal: Specify exactly 1 argument" {:args args})))

    (let [hash (first (:args options))
          dir-name (subs hash 0 2)
          file-name (subs hash 2)
          ^bytes content (-> (fs/file objects-dir dir-name file-name)
                             fs/read-all-bytes)
          blob (zlib-decompress content)
          sepinx (->> blob
                      (keep-indexed
                       (fn [inx elm] (when (= elm 0) inx)))
                      first)
          out (. System out)]
      (if (:p options)
        (. out write blob (inc sepinx) (- (alength blob) (inc sepinx)))
        (throw (ex-info "fatal: Required `-p' option" {:args args}))))))

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

(defn cmd-parse-index
  "Parse .git/index"
  [& _args]
  (-> (fs/file git-dir "index")
      fs/read-all-bytes
      parse-index
      pprint/pprint))

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
              "cat-file" #'cmd-cat-file
              "parse-index" #'cmd-parse-index
              "help" #'cmd-help})

(defn -main [& args]
  (let [action (get actions (first args))]
    (if action
      (apply action args)
      (do
        (println (format "`%s` is not a mgit action.\n" (first args)))
        (cmd-help args)))))
