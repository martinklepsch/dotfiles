-- A global variable for the Hyper Mode
-- k = hs.hotkey.modal.new({"cmd","alt","shift","ctrl"}, nil)

-- Trigger existing hyper key shortcuts

-- k:bind({}, 'm', nil, function() hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, 'm') end)

-- OR build your own

-- Single keybinding for app launch
singleapps = {
  {'t', 'iTerm'},
  {'x', 'Google Chrome'},
  {'e', 'Emacs'},
  -- {'e', 'Atom'},
  {'c', 'Telegram'},
  {'s', 'Slack'},
  {'r', 'Riot'},
  -- {'n', 'Notion'},
}

for i, app in ipairs(singleapps) do
   hs.hotkey.bind(
      {"cmd","alt","shift","ctrl"},
      app[1],
      nil,
      function() hs.application.launchOrFocus(app[2]); end)
end

hs.hotkey.bind(
   {"cmd","alt","shift","ctrl"},
   "i",
   nil,
   function() hs.execute("screencapture -i ~/Dropbox/inspiration/shot_`date '+%Y-%m-%d_%H-%M-%S'`.png"); end)

-- Sequential keybindings, e.g. Hyper-a,f for Finder
-- a = hs.hotkey.modal.new({}, "F16")
-- apps = {
--   {'d', 'Twitter'},
--   {'f', 'Finder'},
--   {'s', 'Skype'},
-- }
-- for i, app in ipairs(apps) do
--   a:bind({}, app[1], function() launch(app[2]); a:exit(); end)
-- end

-- pressedA = function() a:enter() end
-- releasedA = function() end
-- k:bind({}, 'a', nil, pressedA, releasedA)

-- Shortcut to reload config

-- ofun = function()
--   hs.reload()
--   hs.alert.show("Config loaded")
--   k.triggered = true
-- end
-- k:bind({}, 'o', nil, ofun)

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
-- pressedF18 = function()
--   k.triggered = false
--   k:enter()
-- end

-- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed,
--   send ESCAPE if no other keys are pressed.
-- releasedF18 = function()
--   k:exit()
--   if not k.triggered then
--     hs.eventtap.keyStroke({}, 'ESCAPE')
--   end
-- end

-- Bind the Hyper key
-- f18 = hs.hotkey.bind({}, 'F18', pressedF18, nil)

-- Cursor locator

local mouseCircle = nil
local mouseCircleTimer = nil

-- HYPER+L: Open news.google.com in the default browser
-- lfun = function()
--   news = "app = Application.currentApplication(); app.includeStandardAdditions = true; app.doShellScript('open http://news.google.com')"
--   hs.osascript.javascript(news)
--   k.triggered = true
-- end
-- k:bind('', 'l', nil, lfun)
