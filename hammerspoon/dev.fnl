(local hyper [:cmd :alt :shift :ctrl])

(fn reload_config []
  (hs.openConsole)
  (hs.reload)
  (hs.alert.show "Config loaded"))

{:init (fn []
         (print "Installing reload config hotkey Hyper+\\")
         (hs.hotkey.bind hyper "\\" nil reload_config))}
