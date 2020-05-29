-- A global variable for the Hyper Mode
-- k = hs.hotkey.modal.new({"cmd","alt","shift","ctrl"}, nil)
hyper = {"cmd","alt","shift","ctrl"}

-- Trigger existing hyper key shortcuts

-- k:bind({}, 'm', nil, function() hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, 'm') end)

-- OR build your own

-- Single keybinding for app launch
singleapps = {
  {'t', 'iTerm'},
  {'d', 'Todoist'},
  {'v', 'Google Chrome'},
  {'x', 'Brave Browser'},
  -- {'x', 'Safari'},
  -- {'e', 'Visual Studio Code'},
  -- {'e', 'Xcode'},
  -- {'e', 'Emacs'},
  {'e', 'Alacritty'},
  {'n', 'Bear'},
  {'m', 'Mail'},
  {'c', 'Telegram'},
  {'s', 'Slack'},
  {'r', 'Riot'},
  -- {'n', 'Notion'},
}

for i, app in ipairs(singleapps) do
   hs.hotkey.bind(hyper, app[1], nil, function() hs.application.launchOrFocus(app[2]); end)
end

insp = function()
	hs.execute("screencapture -i ~/Dropbox/inspiration/shot_`date '+%Y-%m-%d_%H-%M-%S'`.png");
end
hs.hotkey.bind(hyper, "i", nil, insp)

hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall.use_syncinstall = true
-- https://github.com/scottwhudson/Lunette/
hs.loadSpoon("Lunette")
spoon.Lunette:bindHotkeys()

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
DefaultBrowser = "com.brave.Browser"
IcebreakerBrowser = "com.google.Chrome"
Zoom = "us.zoom.xos"

-- pressedA = function() a:enter() end
-- releasedA = function() end
-- k:bind({}, 'a', nil, pressedA, releasedA)
spoon.SpoonInstall:andUse("URLDispatcher", {
	config = {
		url_patterns = {
			{ "sentry.*icebreaker", IcebreakerBrowser },
			{ "github.*icebreaker", IcebreakerBrowser },
			{ "github.*icebreaker", IcebreakerBrowser },
			{ "https://.*asana.com",  IcebreakerBrowser },
			{ "https://geekbot.com",  IcebreakerBrowser },
			{ "datastudio.google.com", IcebreakerBrowser },
			{ "analytics.amplitude.com", IcebreakerBrowser },
			{ "loom.com", IcebreakerBrowser },
			{ "https://zoom.us/j*",  Zoom },
		},
		default_handler = DefaultBrowser
	},
	start = true
})

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
--
--

-- not really what I want but something
-- https://chris.zarate.org/create-custom-macos-menu-bar-apps-using-hammerspoon
-- https://github.com/GeneralSarsby/asciiIcons/issues/1
local iconAscii = [[ASCII:
............
............
....AD......
..F.....PQ..
..I.........
..........G.
..........H.
.K..........
.N..........
.........L..
..BC.....M..
......SR....
............
............
]]

local automationMenu = hs.menubar.new():setIcon(iconAscii)
local work_mode = false
function menuTable()
	return	{
		{ title = "work mode", fn = function() work_mode = not work_mode end , checked = work_mode} ,
		-- separator { title = "-" },
	}
end

automationMenu:setMenu(menuTable)
