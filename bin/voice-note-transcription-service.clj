#!/usr/bin/env bb

(require '[babashka.pods :as pods]
         '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[clojure.string :as string]
         '[clojure.java.io :as io])

;; Load required pods
(pods/load-pod 'org.babashka/go-sqlite3 "0.1.0")
(pods/load-pod 'org.babashka/fswatcher "0.0.5")



(require '[pod.babashka.go-sqlite3 :as sqlite]
         '[pod.babashka.fswatcher :as fw]
         '[babashka.http-client :as http]
         '[clojure.java.io :as io]
         '[cheshire.core :as json])

(def db-file (str (fs/expand-home "~/etc/bin/file_watcher.db")))
(def memos-dir "/Users/martinklepsch/Library/Mobile Documents/iCloud~md~obsidian/Documents/Personal/_memos")
(def watch-dir (str memos-dir "/attachments"))
(def memos-file (str memos-dir "/Voice Memos.md"))
(def obsidian-url "obsidian://open?vault=Personal&file=_memos%2FVoice%20Memos")

;; Initialize database
(defn init-db! []
  (sqlite/execute! db-file
                   ["CREATE TABLE IF NOT EXISTS files (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        path TEXT NOT NULL,
        sha256 TEXT NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        transcription TEXT,
        transcribed_at TIMESTAMP,
        error TEXT
      );"]))

(defn file-exists-in-db? [path]
  (let [results (sqlite/query db-file
                              ["SELECT * FROM files WHERE path = ?" path])]
    (prn results)
    (-> results first some?)))

(defn add-file-to-db! [path]
  (prn path)
  (when (and (fs/regular-file? path)           ; Only process regular files
             (not (file-exists-in-db? path)))  ; Don't process if already in DB
    (let [sha256 (:out (p/shell {:out :string} "shasum" "-a" "256" path))
          created-at (.lastModified (io/file path))]
      (sqlite/execute! db-file
                       ["INSERT INTO files (path, sha256, created_at) 
                        VALUES (?, ?, datetime(?, 'unixepoch'))"
                        path
                        (first (string/split sha256 #" "))  ; Extract just the hash
                        (quot created-at 1000)])            ; Convert ms to seconds
      (println "✅ Added file to database:" path))))

(def openai-api-key
  (string/trim
   (:out (p/shell
          {:out :string}
          "op item get 'OpenAI Key Voice Note Transcription' --fields credential"))))

(defn transcribe-audio! [file-path]
  (println "⏺️ Transcribing audio file:" file-path)
  (try
    (let [response (http/post "https://api.openai.com/v1/audio/transcriptions"
                              {:headers {"Authorization" (str "Bearer " openai-api-key)}
                               :multipart [{:name "model"
                                            :content "whisper-1"}
                                           {:name "file"
                                            :content (io/file file-path)}]})]
      (if (= 200 (:status response))
        (-> response :body (json/parse-string true) :text)
        (throw (ex-info "Transcription failed" response))))
    (catch Exception e
      (println "🔴 Error transcribing file:" (ex-message e))
      (prn e)
      nil)))

(defn file-modified-str [file-path]
  (let [instant (java.time.Instant/ofEpochMilli (.lastModified (io/file file-path)))
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
        local-dt (.atZone instant (java.time.ZoneId/systemDefault))
        formatted (.format formatter local-dt)
        [date time] (string/split formatted #" ")]
    (str "[[" date "]] " time)))

(def prompt
  (str "I recorded this voice note recently. Please summarize it in the original language and from my perspective.\n
Start the summary with a short one-liner to help me remember the context. Shorter is better.\n
Use a bullet list format, combining related items into single bullets as appropriate\n
Before the bullet list add a short, single-line summary.\n"))

(defn process-file! [path]
  (println "Processing file:" path)
  (try
    (when-let [transcription (transcribe-audio! path)]
      ;; Create a markdown file with the transcription
      (let [file-modified-str (file-modified-str path)
            [summary & body] (->> @(p/process {:out :string
                                               :in (str prompt "\n\n" transcription)}
                                              "llm")
                                  :out
                                  string/split-lines)
            body (string/join "\n" body)
            original-content (when (.exists (io/file memos-file))
                               (slurp memos-file))
            audio-file (str "![[" (fs/file-name path) "]]")]
        (spit memos-file
              (str "## " file-modified-str " " summary "\n\n"
                   audio-file "\n"
                   body "\n\n"
                   original-content))
        (println "✅ Captured transcription to file")
        (p/shell "terminal-notifier -title 'Voice Memos' -message 'Transcription complete' -open" obsidian-url)
        ;; Update database with successful transcription
        (sqlite/execute! db-file
                         ["UPDATE files SET transcription = ?, transcribed_at = CURRENT_TIMESTAMP, error = NULL WHERE path = ?"
                          transcription path])))
    (catch Exception e
      (println "🔴 Error processing file:" (ex-message e))
      (p/shell "terminal-notifier -title 'Voice Memos' -message 'Transcription failed' -open" obsidian-url)
      ;; Update database with error information
      (sqlite/execute! db-file
                       ["UPDATE files SET error = ?, transcribed_at = CURRENT_TIMESTAMP WHERE path = ?"
                        (ex-message e) path]))))

(defn handle-fs-event! [event]
  (let [path (str (:path event))]
    (when (and (= :create (:type event))
               (.endsWith path ".m4a"))
      (add-file-to-db! path)
      (process-file! path))))

(defn start-watcher! []
  (println "Initializing database...")
  (init-db!)

  (println "Starting file watcher in directory:" (fs/absolutize watch-dir))
  (println "Press Ctrl+C to stop")

  (fw/watch watch-dir handle-fs-event! {:delay-ms 1000})

  ;; Keep the script running
  @(promise))

(when (= *file* (System/getProperty "babashka.file"))
  (start-watcher!))