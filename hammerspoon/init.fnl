(local view (require :fennelview))
(local {:pp pp} (require :pp))
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

(fn on_caffeinate_ev [ev_type]
  (if (= ev_type hs.caffeinate.watcher.screensDidUnlock)
    (pp "screens unlocked")))

(local wake_watcher (hs.caffeinate.watcher.new on_caffeinate_ev))
(wake_watcher:start)

(: (require :dev) :init)
(: (require :urlapps) :init)
(: (require :bindings) :init)
(: (require :modes) :init)
