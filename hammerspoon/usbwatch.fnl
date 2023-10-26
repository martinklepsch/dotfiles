(fn watcher-callback [data]
  (print (. data :productName))
  (when (and (= "added" (. data :eventType))
             (= "Keystation 88 MK3" (. data :productName)))
    (hs.alert.show "Starting Piano Practice Session")
    (if (hs.open "/Users/martinklepsch/Music/GarageBand/Piano Practice.band")
      (print "Success!")
      (print "Error opening Piano Practice session"))))

{:init (fn []
         (let [watcher (hs.usb.watcher.new watcher-callback)]
           (watcher:start)))}
