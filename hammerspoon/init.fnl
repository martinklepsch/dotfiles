(set hs.logger.defaultLogLevel "info")
(global logger (hs.logger.new "fennel" "debug"))
(local {:application app :hotkey hotkey} hs) ; destructuring

; hs.logger:i("test")
; use the SpoonInstall Spoon easy installing+loading of Spoons
; (hs.loadSpoon :SpoonInstall)
; (local install (. spoon :SpoonInstall))

; ; for window sizing, use the WIndowHalfsAndThirds Spoon until I can write something custom
; (: install :andUse :WindowHalfsAndThirds)

; ; just bind the default hotkeys for now
; (: spoon.WindowHalfsAndThirds :bindHotkeys
;    (. spoon :WindowHalfsAndThirds :defaultHotkeys))

; launch/focus/activate kitty terminal
(local hyper [:cmd :alt :shift :ctrl])

(hotkey.bind [:cmd :ctrl] :t
             "😾 terminal"
             (partial app.open "terminal"))

(hotkey.bind hyper :r
             "😾 reload" ;somehow not displayed like for terminal one above
             (fn reload []
               (let [x (hs.reload)]
                 (logger.i "reload return" x))))
