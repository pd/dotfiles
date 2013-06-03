##
# Anchored Windows
#
# Bindings:
#
# Next Screen              : cmd-alt-ctrl + N
# Shift to next anchor     : cmd-alt-ctrl + <arrow>
# Scale relative to anchor : cmd+alt + <arrow>
##

mash = ["cmd", "alt", "ctrl"]
mini_mash = ["ctrl", "alt"]

grid_width = 3
grid_height = 2

api.settings().alertAnimates = false
api.settings().alertDisappearDelay = 3

require('~/.zephyros/api.coffee')

# throw to next screen
bind "N", mash, ->
  win = api.focusedWindow()
  screen = win.screen()
  win.shiftAnchorPoint(0, 0, screen.nextScreen())

bind "F", mash, ->
  api.focusedWindow().maximize()

bind "R", mash, ->
  reloadConfig()

bind "h", mash, ->
  api.focusedWindow().shiftAnchorPoint -1, 0

bind "l", mash, ->
  api.focusedWindow().shiftAnchorPoint 1, 0

bind "k", mash, ->
  api.focusedWindow().shiftAnchorPoint 0, -1

bind "j", mash, ->
  api.focusedWindow().shiftAnchorPoint 0, 1

bind "left", mash, ->
  api.focusedWindow().scaleCardinal('west')

bind "right", mash, ->
  api.focusedWindow().scaleCardinal('east')

bind "up", mash, ->
  api.focusedWindow().scaleCardinal('north')

bind "down", mash, ->
  api.focusedWindow().scaleCardinal('south')

bind "h", mini_mash, -> api.focusedWindow().focusWindowLeft()
bind "j", mini_mash, -> api.focusedWindow().focusWindowUp()
bind "k", mini_mash, -> api.focusedWindow().focusWindowDown()
bind "l", mini_mash, -> api.focusedWindow().focusWindowRight()
