--- vim: ts=2 sw=2 et
-- A global variable for the Hyper Mode
-- k = hs.hotkey.modal.new({"cmd","alt","shift","ctrl"}, nil)
hyper = {"cmd","alt","shift","ctrl"}

-- Trigger existing hyper key shortcuts

-- k:bind({}, 'm', nil, function() hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, 'm') end)

insp = function()
  hs.execute("screencapture -i ~/Dropbox/inspiration/shot_`date '+%Y-%m-%d_%H-%M-%S'`.png");
end
hs.hotkey.bind(hyper, "i", nil, insp)

hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall.use_syncinstall = true
-- https://github.com/scottwhudson/Lunette/
hs.loadSpoon("Lunette")
spoon.Lunette:bindHotkeys()

require "deps"

-- FENNEL WHOOOT

local fennel = require "fennel"
-- allow requiring of fennel modules
table.insert(package.loaders or package.searchers, fennel.searcher)
fennel.dofile("init.fnl", { allowedGlobals = false })
