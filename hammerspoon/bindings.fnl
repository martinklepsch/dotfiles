(local {:pp pp} (require :pp))
(local {:application app :hotkey hotkey} hs)

;; -- App bindings

;; State
(var !!installed_bindings {})

;; Helpers
(local hyper [:cmd :alt :shift :ctrl])

(fn app [name]
  (fn [] (hs.application.launchOrFocus name)))

(fn bindings_spec [mode]
  [{:handler (app "iTerm") :key :t}
   {:handler (app "Brave Browser") :key :x}
   {:handler (app "Reminders") :key :d}
   {:handler (app "1Password 7") :key :p}
   {:handler (app "Alacritty") :key :e}
   ; {:handler (app "kitty") :key :e}
   ; {:handler (app "Notion") :key :n}
   {:handler (app "Notes") :key :n}
   {:handler (app "Telegram") :key :c}
   (when (= :work mode)
     {:handler (app "Figma") :key :f})
   (when (= :work mode)
     {:handler (app "Google Chrome") :key :v})
   (when (= :work mode)
     {:handler (app "Slack") :key :s})])

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
