(local view (require :fennelview))
(global pp (fn [x] (print (view x))))
(set hs.logger.defaultLogLevel "info")
(global logger (hs.logger.new "fennel" "debug"))
(local {:application app :hotkey hotkey} hs) ; destructuring

; use the SpoonInstall Spoon easy installing+loading of Spoons
; (hs.loadSpoon :SpoonInstall)
; (local install (. spoon :SpoonInstall))

; ; for window sizing, use the WIndowHalfsAndThirds Spoon until I can write something custom
; (: install :andUse :WindowHalfsAndThirds)

; ; just bind the default hotkeys for now
; (: spoon.WindowHalfsAndThirds :bindHotkeys
;    (. spoon :WindowHalfsAndThirds :defaultHotkeys))

(local hyper [:cmd :alt :shift :ctrl])

; launch/focus/activate kitty terminal
(hotkey.bind [:cmd :ctrl] :t
             "😾 terminal"
             (partial app.open "terminal"))

(hotkey.bind hyper :r
             "😾 reload" ;somehow not displayed like for terminal one above
             (fn reload []
               (let [x (hs.reload)]
                 (logger.i "reload return" x))))

(fn app [name]
  (fn [] (hs.application.launchOrFocus name)))

(fn bindings_spec [off_work_mode]
  [{:handler (app "iTerm") :key :t}
   {:handler (app "Brave Browser") :key :x}
   {:handler (app "Todoist") :key :d}
   {:handler (app "Alacritty") :key :e}
   {:handler (app "Bear") :key :n}
   {:handler (app "Telegram") :key :c}
   (when (not off_work_mode)
     {:handler (app "Google Chrome") :key :v})
   (when (not off_work_mode)
     {:handler (app "Slack") :key :s})
   ])

(var bindings_state (bindings_spec false))

(fn install_bindings [bindings]
  ;; delete installed hotkeys
  (each [_ {:hotkey hk} (ipairs bindings_state)]
    (when hk (print "disabling") (hk:delete)))
  ;; clear state
  (set bindings_state bindings)
  ;; install new bindings
  (each [n bi (ipairs bindings)]
    (print (. bi :app) (. bi :key) (. bi :hotkey))
    (tset bi :hotkey (hotkey.bind hyper (. bi :key) nil (. bi :handler)))))

(install_bindings bindings_state)

; modal productivity
; distraction strategy: disconnect desire and action
; -> log stuff and review instead of instantly looking at it
; -> have super easy interface to capture stuff and resurface at relevant time (i.e. when switching to a different mode)
; modes: custom purpose environments
; -> bind keys to apps diffently (show message about work mode when pressed)
; -> tip: try to use consistent mappings across modes
; -> track time active in different modes
; -> provide a quick switcher to switch between mode (menu bar could display text as reminder)
; -> run more or less arbitrary stuff to enter and switch between work modes
;    -> block distracting websites
;    -> open specific websites

; work mode menubar app
(local automation_menu (hs.menubar.new))
(var off_work_mode false)

(fn switch_work_mode []
  (set off_work_mode (not off_work_mode))
  (: automation_menu :setTitle (if off_work_mode "off work" "work"))
  (install_bindings (bindings_spec off_work_mode)))

(fn menu_table []
  [{:title "off work mode" :fn switch_work_mode :checked off_work_mode}])

(: automation_menu :setTitle "work")
(: automation_menu :setMenu menu_table)
