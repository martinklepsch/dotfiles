(local {:pp pp} (require :pp))
(local {:application app :hotkey hotkey} hs)

;; -- App bindings

;; State
(var !!installed_bindings {})

;; Helpers
(local hyper [:cmd :alt :shift :ctrl])

(fn launch-app [name]
  (hs.application.launchOrFocus name))

(fn open-daily-note []
  (hs.osascript.applescriptFromFile "/Users/martinklepsch/.bin/daily-note.scpt"))

(fn bindings_spec [mode]
  [{:handler #(launch-app "iTerm") :key :t}
   {:handler #(launch-app "Brave Browser") :key :x}
   (if (= :music mode)
     {:handler #(launch-app "Freeform") :key :e}
     {:handler #(launch-app "Alacritty") :key :e})
   {:handler open-daily-note :key :d}
   {:handler #(launch-app "1Password 7") :key :p}
   {:handler #(launch-app "portal") :key :l}
   ; {:handler #(launch-app "kitty") :key :e}
   ; {:handler #(launch-app "Notion") :key :n}
   {:handler #(launch-app "Notes") :key :n}
   {:handler #(launch-app "Telegram") :key :c}
   (if (= :work mode)
     {:handler #(launch-app "Figma") :key :f}
     {:handler #(launch-app "Freeform") :key :f})
   (when (= :work mode)
     {:handler #(launch-app "Google Chrome") :key :v})
   (when (= :work mode)
     {:handler #(launch-app "Slack") :key :s})])

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

;; API
{:init (fn [] (install_bindings (bindings_spec false)))
 :set (fn [mode] (install_bindings (bindings_spec mode)))}
