## Divide a screen into WxH grid
class Grid
  constructor: (columns, rows, margin, screen) ->
    @columns = parseFloat(columns)
    @rows    = parseFloat(rows)
    @margin  = if margin? then parseInt(margin) else 5
    @screen  = if screen? then screen else Screen.mainScreen

    rect = @screen.frameWithoutDockOrMenu
    @minX = NSMinX(rect)
    @minY = NSMinY(rect)
    @cellWidth = rect.size.width / @columns
    @cellHeight = rect.size.height / @rows

  onScreen: (screen) -> new Grid(@columns, @rows, @margin, screen)

  locate: (win) ->
    frame = win.frame
    new Grid.Location(this,
                      Math.round((frame.origin.x - @minX) / @cellWidth),
                      Math.round((frame.origin.y - @minY) / @cellHeight),
                      Math.max(Math.round(frame.size.width / @cellWidth), 1),
                      Math.max(Math.round(frame.size.height / @cellHeight), 1))

  rect: (loc) ->
    CGRectMake(loc.column * @cellWidth + @minX, loc.row * @cellHeight + @minY,
               loc.width * @cellWidth, loc.height * @cellHeight)

  place: (win, loc) ->
    rect = @rect(loc)
    rect = NSInsetRect(rect, @margin, @margin) if @margin isnt 0
    rect = NSIntegralRect(rect)
    win.setFrame(rect)

## Position of a cell within the grid
class Grid.Location
  constructor: (@grid, @column, @row, @width, @height) ->

  _modify: (fn) ->
    destination = _.clone this
    fn.call(destination, @grid)
    destination

  fillColumn: ->
    @_modify (grid) ->
      @row = 0
      @height = grid.rows
      this

  inc: (widthOrHeight, delta) ->
    @_modify (grid) ->
      switch widthOrHeight
        when 'width'  then @width = Math.min(grid.columns - @column, Math.max(@width + delta, 1));
        when 'height' then @height = Math.min(grid.rows - @row, Math.max(@height + delta, 1));
      this

  move: (columnOrRow, delta) ->
    dest = @_modify (grid) ->
      switch columnOrRow
        when 'column' then @column = Math.min(grid.columns - 1, Math.max(@column + delta, 0))
        when 'row'    then @row = Math.min(grid.rows - 1, Math.max(@row + delta, 0))
      this
    dest.constrainToScreen()

  constrainToScreen: ->
    @_modify (grid) ->
      @width  = grid.columns - @column if @column + @width > grid.columns
      @height = grid.rows - @row if @row + @height > grid.rows
      this

  toString: ->
    "[loc (#{@column},#{@row}) [#{@width}x#{@height}]]"

## Helper fns to tidy up
bindings = (modifiers, keys) ->
  bind key, modifiers, callback for key, callback of keys

focused = (callback) ->
  () ->
    win = Win.focusedWindow
    callback(win) if win?

onGrid = (target, callback) ->
  focused (win) ->
    grid = target.onScreen win.screen
    loc  = grid.locate win
    callback.call(this, grid, win, grid.locate(win), win.screen)

relocate = (callback) ->
  onGrid defaultGrid, (grid, win, location) ->
    grid.place win, callback.call(this, location)

moveToScreen = (callback) ->
  onGrid defaultGrid, (grid, win, location, screen) ->
    otherScreen = callback.call(this, screen)
    target = grid.onScreen otherScreen
    loc    = _.extend(_.clone(location), grid: target)
    target.place(win, loc)

combos =
  mash:  ["CTRL", "ALT", "CMD"],
  cmdFn: ["CTRL", "ALT"] # TODO ["CMD", "FN"] once FN works

defaultGrid = new Grid(3, 2, 2)

bindings combos.mash,
  'R': -> App.reloadConfig
  'F': focused (win) -> win.maximize

  'U': relocate (loc) -> loc.fillColumn()

  'H': relocate (loc) -> loc.move('column', -1)
  'J': relocate (loc) -> loc.move('row', 1)
  'K': relocate (loc) -> loc.move('row', -1)
  'L': relocate (loc) -> loc.move('column', 1)

  'I': relocate (loc) -> loc.inc('width', -1)
  'O': relocate (loc) -> loc.inc('width', 1)

  '\\': moveToScreen (screen) -> screen.nextScreen

bindings combos.cmdFn,
  'H': focused (win) -> win.focusWindowLeft
  'J': focused (win) -> win.focusWindowDown
  'K': focused (win) -> win.focusWindowUp
  'L': focused (win) -> win.focusWindowRight
