# See https://github.com/jasonrudolph/keyboard
# and http://stevelosh.com/blog/2012/10/a-modern-space-cadet
# for converting Option_R to 'Hyper' in the form of MASH+SHIFT
hyper = ["cmd", "alt", "ctrl", "shift"]

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
bind "/", hyper, ->
  win = api.focusedWindow()
  screen = win.screen()
  win.shiftAnchorPoint(0, 0, screen.nextScreen())

bind "g", hyper, -> api.focusedWindow().maximize()
bind "r", hyper, -> reloadConfig()

bind "h", hyper, -> shift -1,  0
bind "l", hyper, -> shift  1,  0
bind "k", hyper, -> shift  0, -1
bind "j", hyper, -> shift  0,  1

bind 'a', hyper, -> focus 'Left'
bind 's', hyper, -> focus 'Down'
bind 'd', hyper, -> focus 'Up'
bind 'f', hyper, -> focus 'Right'

bind 'left',  hyper, -> scale 'west'
bind 'right', hyper, -> scale 'east'
bind 'up',    hyper, -> scale 'north'
bind 'down',  hyper, -> scale 'south'
