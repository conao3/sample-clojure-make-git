(ns mgit.mgit
  (:require
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

(defn read-uint32 [^bytes data offset]
  (bit-or (bit-shift-left (bit-and (aget data offset) 0xff) 24)
          (bit-shift-left (bit-and (aget data (+ offset 1)) 0xff) 16)
          (bit-shift-left (bit-and (aget data (+ offset 2)) 0xff) 8)
          (bit-and (aget data (+ offset 3)) 0xff)))

(defn read-uint16 [^bytes data offset]
  (bit-or (bit-shift-left (bit-and (aget data offset) 0xff) 8)
          (bit-and (aget data (+ offset 1)) 0xff)))

(defn read-string [^bytes data offset len]
  (String. data offset len "UTF-8"))

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

(defn parse-index-entry [^bytes data offset]
  (let [ctime-sec (read-uint32 data offset)
        ctime-nsec (read-uint32 data (+ offset 4))
        mtime-sec (read-uint32 data (+ offset 8))
        mtime-nsec (read-uint32 data (+ offset 12))
        dev (read-uint32 data (+ offset 16))
        ino (read-uint32 data (+ offset 20))
        mode (read-uint32 data (+ offset 24))
        uid (read-uint32 data (+ offset 28))
        gid (read-uint32 data (+ offset 32))
        file-size (read-uint32 data (+ offset 36))
        sha1 (format "%040x"
                    (BigInteger. 1 (java.util.Arrays/copyOfRange data
                                                                (+ offset 40)
                                                                (+ offset 60))))
        flags (read-uint16 data (+ offset 60))
        name-len (bit-and flags 0xfff)
        name-offset (+ offset 62)
        name (read-string data name-offset name-len)
        entry-size (+ 62 name-len (- 8 (mod (+ 62 name-len) 8)))]
    {:entry {:ctime-sec ctime-sec
             :ctime-nsec ctime-nsec
             :mtime-sec mtime-sec
             :mtime-nsec mtime-nsec
             :dev dev
             :ino ino
             :mode mode
             :uid uid
             :gid gid
             :file-size file-size
             :sha1 sha1
             :flags flags
             :name name}
     :entry-size entry-size}))

(defn parse-index [^bytes data]
  (let [header (read-string data 0 4)]
    (when-not (= header "DIRC")
      (throw (ex-info "Invalid index file format" {:header header})))

    (let [version (read-uint32 data 4)
          entries-count (read-uint32 data 8)
          entries (loop [offset 12
                         entries []
                         remaining entries-count]
                    (if (zero? remaining)
                      entries
                      (let [{:keys [entry entry-size]} (parse-index-entry data offset)]
                        (recur (+ offset entry-size)
                               (conj entries entry)
                               (dec remaining)))))]
      {:version version
       :entries-count entries-count
       :entries entries})))

(defn create-index-entry [file-path hash stat]
  (let [stat-dev (or (:dev stat) 0)
        stat-ino (or (:ino stat) 0)
        stat-mode (or (:mode stat) 0)
        stat-uid (or (:uid stat) 0)
        stat-gid (or (:gid stat) 0)
        stat-size (or (:size stat) 0)
        ctime (.toEpochSecond (or (:ctime stat) (java.time.Instant/now)))
        ctime-nsec 0
        mtime (.toEpochSecond (or (:mtime stat) (java.time.Instant/now)))
        mtime-nsec 0
        name (str file-path)]
    {:ctime-sec ctime
     :ctime-nsec ctime-nsec
     :mtime-sec mtime
     :mtime-nsec mtime-nsec
     :dev stat-dev
     :ino stat-ino
     :mode stat-mode
     :uid stat-uid
     :gid stat-gid
     :file-size stat-size
     :sha1 hash
     :flags (count name)
     :name name}))

(defn write-uint32 [baos value]
  (. baos write (bit-and (bit-shift-right value 24) 0xff))
  (. baos write (bit-and (bit-shift-right value 16) 0xff))
  (. baos write (bit-and (bit-shift-right value 8) 0xff))
  (. baos write (bit-and value 0xff)))

(defn write-uint16 [baos value]
  (. baos write (bit-and (bit-shift-right value 8) 0xff))
  (. baos write (bit-and value 0xff)))

(defn write-index [index]
  (with-open [baos (ByteArrayOutputStream.)]
    (let [entries-count (count (:entries index))]
      ;; Write header
      (. baos write (. "DIRC" getBytes "UTF-8"))
      (write-uint32 baos (:version index))
      (write-uint32 baos entries-count)

      ;; Write entries
      (doseq [entry (sort-by :name (:entries index))]
        (write-uint32 baos (:ctime-sec entry))
        (write-uint32 baos (:ctime-nsec entry))
        (write-uint32 baos (:mtime-sec entry))
        (write-uint32 baos (:mtime-nsec entry))
        (write-uint32 baos (:dev entry))
        (write-uint32 baos (:ino entry))
        (write-uint32 baos (:mode entry))
        (write-uint32 baos (:uid entry))
        (write-uint32 baos (:gid entry))
        (write-uint32 baos (:file-size entry))

        ;; Write SHA-1
        (let [sha1-bytes (. (BigInteger. (:sha1 entry) 16) toByteArray)]
          (. baos write (byte-array (- 20 (alength sha1-bytes))))
          (. baos write sha1-bytes))

        ;; Write flags & name
        (let [name (:name entry)
              name-len (count name)]
          (write-uint16 baos (bit-and name-len 0xfff))
          (. baos write (. name getBytes "UTF-8"))

          ;; Padding to multiple of 8 bytes
          (let [padding-size (- 8 (mod (+ 62 name-len) 8))]
            (when (< padding-size 8)
              (. baos write (byte-array padding-size))))))

      ;; Return the index data
      (. baos toByteArray))))

(defn cmd-update-index
  "Register file into working tree"
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

    (when-not (:add options)
      (throw (ex-info "fatal: Specify `--add' option" {:args args})))

    (let [index-file (fs/file git-dir "index")
          index (if (fs/exists? index-file)
                  (-> index-file
                      fs/read-all-bytes
                      parse-index)
                  {:version 2
                   :entries-count 0
                   :entries []})
          entries (:entries index)]

      (doseq [file-path (:args options)]
        (let [file (fs/file file-path)
              content (fs/read-all-bytes file)
              stat (fs/file-attributes file)
              ;; Create blob object
              blob-content (with-open [byte-stream (ByteArrayOutputStream.)
                                       writer (OutputStreamWriter. byte-stream "UTF-8")]
                             (. writer write (format "blob %s\0" (count content)))
                             (. writer flush)
                             (. byte-stream write content)
                             (. byte-stream toByteArray))
              hash (digest/sha1 blob-content)
              dir-name (subs hash 0 2)
              file-name (subs hash 2)
              file-path-in-objects (fs/file objects-dir dir-name file-name)]

          ;; Store blob
          (when-not (fs/exists? file-path-in-objects)
            (fs/create-dirs (fs/parent file-path-in-objects))
            (fs/write-bytes file-path-in-objects (zlib-compress blob-content)))

          ;; Create index entry
          (let [entry (create-index-entry file-path hash stat)
                existing-entry-index (when entries
                                       (first (keep-indexed
                                               (fn [idx itm] (when (= (:name itm) file-path) idx))
                                               entries)))
                new-entries (if existing-entry-index
                              (assoc entries existing-entry-index entry)
                              (conj (or entries []) entry))
                new-index {:version (:version index)
                          :entries-count (count new-entries)
                          :entries new-entries}]

            ;; Write index file
            (fs/write-bytes index-file (write-index new-index))
            (println (format "added '%s'" file-path))))))))

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
              "update-index" #'cmd-update-index
              "help" #'cmd-help})

(defn -main [& args]
  (let [action (get actions (first args))]
    (if action
      (apply action args)
      (do
        (println (format "`%s` is not a mgit action.\n" (first args)))
        (cmd-help args)))))
