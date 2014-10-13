-- local application = require "mjolnir.application"
-- local fnutils = require "mjolnir.fnutils"
-- local screen = require "mjolnir.screen"

local window = require "mjolnir.window"
local hotkey = require "mjolnir.hotkey"
local grid = require "mjolnir.sd.grid"

local mash = {"ctrl", "cmd", "shift"}
local malt = {"ctrl", "alt", "shift"}

grid.MARGINX = 0
grid.MARGINY = 0
grid.GRIDWIDTH = 2

hotkey.bind(mash, "c", mjolnir.openconsole)
hotkey.bind(mash, "r", mjolnir.reload)

-- ctrl+cmd+shift+<foo> => navigate
hotkey.bind(mash, "h", function() window.focusedwindow():focuswindow_west()  end)
hotkey.bind(mash, "j", function() window.focusedwindow():focuswindow_south() end)
hotkey.bind(mash, "k", function() window.focusedwindow():focuswindow_north() end)
hotkey.bind(mash, "l", function() window.focusedwindow():focuswindow_east()  end)

-- ctrl+alt+shift+<foo> => resize / move around
local function flushleft()
   local win = window.focusedwindow()
   grid.set(win, { x = 0, y = 0, w = 1, h = 2 }, win:screen())
end

local function flushright()
   local win = window.focusedwindow()
   grid.set(win, { x = 1, y = 0, w = 1, h = 2 }, win:screen())
end

hotkey.bind(malt, "m", grid.maximize_window)
hotkey.bind(malt, "n", grid.pushwindow_nextscreen)
hotkey.bind(malt, "h", flushleft)
hotkey.bind(malt, "l", flushright)
