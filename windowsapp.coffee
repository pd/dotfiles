expandPath = (path) ->
  NSString.stringWithString(path).stringByStandardizingPath

readFile = (file) ->
  path = expandPath file
  NSString.stringWithContentsOfFile_encoding_error(path, NSUTF8StringEncoding, null)

compile = (file) ->
  CoffeeScript.compile readFile(file), bare: true

eval compile "~/dotfiles/windowsapp/grid.coffee"

PopupSettings.disappearDelay = 3.0
defaultGrid = new Grid(3, 2, 2)

bindings = (modifiers, keys) ->
  bind key, modifiers, callback for key, callback of keys

# Yields the currently focused window, if any.
focused = (callback) ->
  () ->
    win = Win.focusedWindow
    callback(win) if win?

# Yields the given grid, currently focused window,
# its grid location, and the screen it belongs to.
onGrid = (target, callback) ->
  focused (win) ->
    grid = target.onScreen win.screen
    loc  = grid.locate win
    callback.call(this, grid, win, grid.locate(win), win.screen)

# Yields the location of the currently focused window.
# The window will be place at the location returned by the
# callback.
relocate = (callback) ->
  onGrid defaultGrid, (grid, win, location) ->
    grid.place win, callback.call(this, location)

# Yields the currently focused window's screen. The
# window is tossed to screen returned by the callback.
tossScreen = (callback) ->
  onGrid defaultGrid, (grid, win, location, screen) ->
    otherScreen = callback.call(this, screen)
    target = grid.onScreen otherScreen
    loc    = _.extend(_.clone(location), grid: target)
    target.place(win, loc)

bindings ["CTRL", "ALT"],
  'H': focused (win) -> win.focusWindowLeft
  'J': focused (win) -> win.focusWindowDown
  'K': focused (win) -> win.focusWindowUp
  'L': focused (win) -> win.focusWindowRight

bindings ["CTRL", "ALT", "CMD"],
  'R': -> App.reloadConfig
  'F': focused (win) -> win.maximize

  'U': relocate (loc) -> loc.fillColumn()

  'H': relocate (loc) -> loc.move('column', -1)
  'J': relocate (loc) -> loc.move('row', 1)
  'K': relocate (loc) -> loc.move('row', -1)
  'L': relocate (loc) -> loc.move('column', 1)

  'I': relocate (loc) -> loc.inc('width', -1)
  'O': relocate (loc) -> loc.inc('width', 1)

  '\\': tossScreen (screen) -> screen.nextScreen

  'D': focused (win) ->
    alert(defaultGrid.onScreen(win.screen).locate(win).toString())
