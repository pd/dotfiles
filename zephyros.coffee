# See https://github.com/jasonrudolph/keyboard
# and http://stevelosh.com/blog/2012/10/a-modern-space-cadet
# for converting Control_L to 'Hyper' in the form of MASH+SHIFT
hyper = ['cmd', 'alt', 'ctrl', 'shift']

grid_width = 3
grid_height = 2

api.settings().alertAnimates = false
api.settings().alertDisappearDelay = 3

require('~/.zephyros/api.coffee')

shift = (xd, yd) ->
  win = api.focusedWindow()
  win?.shiftAnchorPoint xd, yd

scale = (dir) ->
  win = api.focusedWindow()
  win?.scaleCardinal(dir)

focus = (dir) ->
  win = api.focusedWindow()
  win?["focusWindow#{dir}"]()

# throw to next screen
bind '/', hyper, ->
  win = api.focusedWindow()
  screen = win.screen()
  win.shiftAnchorPoint(0, 0, screen.nextScreen())

bind 'm', hyper, -> api.focusedWindow()?.maximize()
bind 'r', hyper, -> reloadConfig()

bind 'h', hyper, -> focus 'Left'
bind 'j', hyper, -> focus 'Down'
bind 'k', hyper, -> focus 'Up'
bind 'l', hyper, -> focus 'Right'

bind 'a', hyper, -> shift -1,  0
bind 's', hyper, -> shift  0,  1
bind 'd', hyper, -> shift  0, -1
bind 'f', hyper, -> shift  1,  0

bind 'left',  hyper, -> scale 'west'
bind 'right', hyper, -> scale 'east'
bind 'up',    hyper, -> scale 'north'
bind 'down',  hyper, -> scale 'south'
