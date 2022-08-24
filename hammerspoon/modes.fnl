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
(local {:set set_bindings} (require :bindings))
(var !!mode :off)

(fn set_mode [new_mode]
  (print "Setting mode" new_mode)
  (set !!mode new_mode)
  (: automation_menu :setTitle new_mode)
  (set_bindings (= :work new_mode)))

(fn menu_table [mode]
  [(if (= :off mode)
     {:title "do work" :fn #(set_mode :work) :checked false}
     {:title "stop work" :fn #(set_mode :work) :checked false})
   (when (not (= :music mode))
     {:title "make music" :fn #(set_mode :music) :checked false}) ])

{:init (fn []
         (: automation_menu :setTitle !!mode)
         (: automation_menu :setMenu #(menu_table !!mode)))}

