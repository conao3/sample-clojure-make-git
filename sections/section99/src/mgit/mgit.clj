(ns mgit.mgit
  (:require
   [babashka.fs :as fs]
   [clj-commons.digest :as digest]
   [clojure.pprint :as pprint]
   [clojure.string :as str])
  (:import
   [java.io ByteArrayOutputStream DataOutputStream OutputStreamWriter]
   [java.nio.file.attribute FileTime]
   [java.time Instant]
   [java.util.zip Deflater Inflater])
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

(def uint8 byte)
(def uint16 short)
(def uint32 int)
(def uint64 long)

(defn int->hexstr [elm]
  (format "%02x" elm))

(defn int->octstr [elm]
  (format "%o" elm))

(defn bytes->int [data]
  (reduce (fn [acc elm] (+ (bit-shift-left acc 8) (bit-and elm 0xff))) 0 data))

(defn bytes->str [data]
  (-> data byte-array String.))

(defn bytes->hexstr [data]
  (->> data ((partial map int->hexstr)) (apply str)))

(defn hexstr->uint32 [data]
  (Integer/parseUnsignedInt data 16))

(defn hexstr->bytes [data]
  (->> data
       (partition 8)
       ((partial map (partial apply str)))
       ((partial map hexstr->uint32))))

(defn bytes-parse [acc key parse-fn remaining-update-fn]
  (-> acc
      (assoc-in [:parsed key] (parse-fn acc))
      (update :remaining remaining-update-fn)))

(defn parse-bytes [acc key size & [update-value-fn]]
  (-> acc
      (bytes-parse key #(take size (:remaining %)) (partial drop size))
      (update-in [:parsed key] (or update-value-fn identity))))

(defn parse-int [acc key size]
  (-> acc
      (parse-bytes key size bytes->int)))

(defn parse-uint16 [acc key]
  (parse-int acc key 2))

(defn parse-uint32 [acc key]
  (parse-int acc key 4))

(defn parse-const [acc key ^String const]
  (bytes-parse acc
               key
               #(do
                  (when-not (= (take 4 (:remaining %))
                               (seq (. const getBytes)))
                    (throw (ex-info "Data should be start specifyed constant" {:acc % :const const})))
                  true)
               (partial drop 4)))

(defn read-until-null [acc]
  (loop [bytes []
         find-null false
         remaining (:remaining acc)]
    (let [d (first remaining)]
      (cond
        (= d 0)
        (recur bytes true (rest remaining))

        (and (not= d 0) (not find-null))
        (recur (conj bytes d) false (rest remaining))

        (and (not= d 0) find-null)
        {:bytes bytes :remaining remaining}))))

(defn camel->kebab-keyword [s]
  (-> s
      name
      (str/replace #"([a-z])([A-Z])" "$1-$2")
      str/lower-case
      keyword))

(defn concat-bytes
  "concat Number, String, bytes into one bytes"
  ^bytes [& args]
  (with-open [byte-stream (ByteArrayOutputStream.)
              data-out (DataOutputStream. byte-stream)
              writer (OutputStreamWriter. byte-stream "UTF-8")]
    (doseq [arg args]
      (cond
        (instance? Byte arg) (.writeByte data-out (byte arg))
        (instance? Short arg) (.writeShort data-out arg)
        (instance? Integer arg) (.writeInt data-out arg)
        (instance? Long arg) (.writeLong data-out arg)

        (instance? String arg)
        (do (.write writer ^String arg)
            (.flush writer))

        (instance? (Class/forName "[B") arg)
        (.write byte-stream ^bytes arg)

        (seqable? arg)
        (.write byte-stream ^bytes (apply concat-bytes arg))

        :else
        (throw (IllegalArgumentException.
                (str "Unsupported type: " (class arg))))))

    (.toByteArray byte-stream)))

(defn blob-blob ^bytes [^bytes data]
  (concat-bytes (format "blob %s\0" (count data)) data))

(defn blob-tree ^bytes [^bytes data]
  (concat-bytes (format "tree %s\0" (count data)) data))

(defn get-file-attrs [file]
  (let [f (fs/file file)
        ;; :creation-time
        ;; :file-key
        ;; :group
        ;; :is-directory
        ;; :is-other
        ;; :is-regular-file
        ;; :is-symbolic-link
        ;; :last-access-time
        ;; :last-modified-time
        ;; :owner
        ;; :permissions
        ;; :size
        posix-attrs (-> (fs/read-attributes f "posix:*")
                        (update-keys camel->kebab-keyword))
        ;; :creation-time
        ;; :ctime
        ;; :dev
        ;; :file-key
        ;; :gid
        ;; :group
        ;; :ino
        ;; :is-directory
        ;; :is-other
        ;; :is-regular-file
        ;; :is-symbolic-link
        ;; :last-access-time
        ;; :last-modified-time
        ;; :mode
        ;; :nlink
        ;; :owner
        ;; :permissions
        ;; :rdev
        ;; :size
        ;; :uid
        unix-attrs (-> (fs/read-attributes f "unix:*")
                       (update-keys camel->kebab-keyword))
        attrs (merge posix-attrs unix-attrs)]
    (-> attrs
        (assoc :filepath file) ; TODO: .gitからの相対パスにする
        (assoc :ctime (-> attrs :creation-time FileTime/.toInstant))
        (assoc :mtime (-> attrs :last-modified-time FileTime/.toInstant))
        (assoc :atime (-> attrs :last-access-time FileTime/.toInstant)))))

(defn get-file-entry [attrs]
  (let [blob (-> attrs :filepath fs/read-all-bytes blob-blob)
        hash (-> blob digest/sha1)
        file-path (fs/file objects-dir (subs hash 0 2) (subs hash 2))]
    (-> file-path fs/parent fs/create-dirs)
    (->> blob zlib-compress (fs/write-bytes file-path))
    (merge
     (select-keys attrs [:dev :ino :mode :uid :gid :size :filepath])
     {:ctime-sec (-> attrs :ctime Instant/.getEpochSecond)
      :ctime-nsec (-> attrs :ctime Instant/.getNano)
      :mtime-sec (-> attrs :mtime Instant/.getEpochSecond)
      :mtime-nsec (-> attrs :mtime Instant/.getNano)
      :object-id hash
      ;; TODO: 実際にはassume-validなどを考慮する必要がある
      :flag (-> attrs :filepath count (min 0xfff))})))

(defn parse-index [^bytes data]
  (let [parse-filepath (fn [acc key]
                         (let [res (read-until-null acc)]
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
        (parse-const :dirc "DIRC")
        (parse-uint32 :version)
        (parse-uint32 :number-of-entries)
        (parse-entries :entries)
        (parse-bytes :checksum 20 bytes->hexstr))))

(defn create-index ^bytes [args]
  (let [blob (concat-bytes
              "DIRC"
              (uint32 (:version args))
              (uint32 (count (:entries args)))
              (->> (:entries args)
                   (map (juxt (comp uint32 :ctime-sec)
                              (comp uint32 :ctime-nsec)
                              (comp uint32 :mtime-sec)
                              (comp uint32 :mtime-nsec)
                              (comp uint32 :dev)
                              (comp uint32 :ino)
                              (comp uint32 :mode)
                              (comp uint32 :uid)
                              (comp uint32 :gid)
                              (comp uint32 :size)
                              #(-> (:object-id %) hexstr->bytes)
                              (comp uint16 :flag)
                              #(-> % :filepath String/.getBytes)
                              ;; 8Byte区切りまでnullで埋める。
                              ;; これまでのエントリでファイルパスの開始時点がずれているため、調整する。
                              #(-> (- 8 (mod (+ 6 (-> % :filepath String/.getBytes count)) 8))
                                   (repeat (uint8 0)))))))]
    (concat-bytes
     blob
     (-> (digest/sha1 blob) hexstr->bytes))))

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
      (let [blob (blob-blob (fs/read-all-bytes filepath))
            hash (digest/sha1 blob)
            file-path (fs/file objects-dir (subs hash 0 2) (subs hash 2))]
        (println hash)
        (when (:w options)
          (-> file-path fs/parent fs/create-dirs)
          (->> blob zlib-compress (fs/write-bytes file-path)))))))

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

(defn cmd-parse-index
  "Parse .git/index"
  [& _args]
  (-> (fs/file git-dir "index")
      fs/read-all-bytes
      parse-index
      pprint/pprint))

(defn cmd-update-index
  "Register file content to the index"
  [& args]
  (let [{:keys [options error]}
        (->> (rest args)
             (parse
              (fn [acc]
                (condp apply [(-> acc :remaining first)]
                  #{"--add"} (-> acc
                                 (assoc-in [:options :add] true)
                                 (update :remaining next))
                  nil))))]
    (when error
      (throw (ex-info error {:args args})))

    (let [index (-> (fs/file git-dir "index")
                    fs/read-all-bytes
                    parse-index
                    :parsed)
          filemap (->> (:entries index)
                       (map (juxt :filepath identity))
                       (into {}))
          res (reduce
               (fn [acc elm]
                 (assoc acc elm (-> elm get-file-attrs get-file-entry)))
               filemap
               (:args options))]
      (-> (fs/file git-dir "index")
          (fs/write-bytes
           (create-index (assoc index :entries (->> res (sort-by key) (map val)))))))))

(defn cmd-write-tree
  "Create tree object from the current index"
  [& _args]
  (let [index (-> (fs/file git-dir "index")
                  fs/read-all-bytes
                  parse-index
                  :parsed)
        blob (->> (:entries index)
                  (map (juxt (comp int->octstr :mode)
                             (constantly " ")
                             :filepath
                             (constantly "\0")
                             (comp hexstr->bytes :object-id)))
                  concat-bytes
                  blob-tree)
        hash (digest/sha1 blob)
        file-path (fs/file objects-dir (subs hash 0 2) (subs hash 2))]
    (println hash)
    (-> file-path fs/parent fs/create-dirs)
    (->> blob zlib-compress (fs/write-bytes file-path))))

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
              "update-index" #'cmd-update-index
              "write-tree" #'cmd-write-tree
              "help" #'cmd-help})

(defn -main [& args]
  (let [action (get actions (first args))]
    (if action
      (apply action args)
      (do
        (println (format "`%s` is not a mgit action.\n" (first args)))
        (cmd-help args)))))
