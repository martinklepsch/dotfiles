(local {:pp pp} (require :pp))
(local {:application app :hotkey hotkey} hs)

;; -- App bindings

;; State
(var !!installed_bindings {})

;; Helpers
(local hyper [:cmd :alt :shift :ctrl])

(fn launch-app [name]
  (hs.application.launchOrFocus name))

(fn cycle-chat []
  (let [front (hs.application.frontmostApplication)]
    (if (= "WhatsApp" (: front :name))
      (launch-app "Messages")
      (= "Telegram" (: front :name))
      (launch-app "WhatsApp")
      ;; :else
      (launch-app "Telegram"))))

(fn vault-id []
  (if (= "mk-ohana" (os.getenv "USER"))
    "b0cf100d1371ef62"
    "4491b0e1c7f829b7"))

(fn open-daily-note []
  ;; #_(hs.osascript.applescriptFromFile "/Users/martinklepsch/.bin/daily-note.scpt")
  ;; (os.execute "open \"obsidian://open?vault=4491b0e1c7f829b7&daily\"")
  (os.execute (.. "open 'obsidian://advanced-uri?vault=" (vault-id) "&commandid=periodic-notes%253Aopen-daily-note'")))

(fn open-weekly-note []
  ;; #_(hs.osascript.applescriptFromFile "/Users/martinklepsch/.bin/daily-note.scpt")
  ;; (os.execute "open \"obsidian://open?vault=4491b0e1c7f829b7&daily\"")
  (os.execute (.. "open 'obsidian://advanced-uri?vault=" (vault-id) "&commandid=periodic-notes%253Aopen-weekly-note'")))


(fn bindings_spec [mode]
  [{:handler #(launch-app "Ghostty") :key :t}
   {:handler #(launch-app "Google Chrome") :key :x}
   (if (= :music mode)
     {:handler #(launch-app "Freeform") :key :e}
     {:handler #(launch-app "kitty") :key :e})
   {:handler open-daily-note :key :d}
   {:handler open-weekly-note :key :w}
   {:handler #(launch-app "1Password 7") :key :p}
   {:handler #(launch-app "portal") :key :l}
   ; {:handler #(launch-app "kitty") :key :e}
   ; {:handler #(launch-app "Notion") :key :n}
   {:handler #(launch-app "Notes") :key :n}
   {:handler #(cycle-chat) :key :c}
   {:handler #(launch-app "Figma") :key :f}
   {:handler #(launch-app "Obsidian") :key :o}
   (when (= :work mode)
     {:handler #(launch-app "Google Chrome") :key :v})
   {:handler #(launch-app "Slack") :key :s}])

(fn install_bindings [bindings]
  ;; delete installed hotkeys
  (each [_ {:hotkey hk} (ipairs !!installed_bindings)]
    (when hk
      ;; (pp hk)
      (hk:delete)))
  ;; clear state
  (set !!installed_bindings bindings)
  ;; install new bindings
  (each [n bi (ipairs bindings)]
    ;; (print (. bi :app) (. bi :key) (. bi :hotkey))
    (tset bi :hotkey (hotkey.bind hyper (. bi :key) nil (. bi :handler)))))

; (fn search_notes []
;   (let [notes (hs.application.find "Notes")]
;     (pp notes)
;     (hs.eventtap.keyStroke [:cmd :alt] :f nil notes)))

; (hotkey.bind hyper :k nil search_notes)

;; API
{:init (fn [] (install_bindings (bindings_spec false)))
 :set (fn [mode] (install_bindings (bindings_spec mode)))}
