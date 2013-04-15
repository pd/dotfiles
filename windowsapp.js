// bindings for https://github.com/sdegutis/Windows

// TODO Find a way to move Grid / Grid.Location elsewhere
// TODO Sucks to have 'defaultGrid' below. Hrmph.

// A screen split into a WxH grid
var Grid = function(columns, rows, margin, screen) {
  this.columns = parseFloat(columns);
  this.rows    = parseFloat(rows);

  if (typeof margin === 'undefined')
    this.margin = 5;
  else
    this.margin = parseInt(margin);

  if (typeof screen === 'undefined')
    this.screen = [Screen mainScreen];
  else
    this.screen = screen;

  var screenRect = [this.screen frameWithoutDockOrMenu];
  this.minX = NSMinX(screenRect);
  this.minY = NSMinY(screenRect);
  this.cellWidth  = screenRect.size.width / this.columns;
  this.cellHeight = screenRect.size.height / this.rows;
};

Grid.prototype.onScreen = function(screen) {
  return new Grid(this.columns, this.rows, this.margin, screen);
};

Grid.prototype.locate = function(win) {
  var frame = [win frame];
  return new Grid.Location(this,
                           Math.round((frame.origin.x - this.minX) / this.cellWidth),
                           Math.round((frame.origin.y - this.minY) / this.cellHeight),
                           Math.max(Math.round(frame.size.width / this.cellWidth), 1),
                           Math.max(Math.round(frame.size.height / this.cellHeight), 1));
};

Grid.prototype.rect = function(location) {
  return CGRectMake(location.column * this.cellWidth + this.minX,
                    location.row * this.cellHeight + this.minY,
                    location.width * this.cellWidth,
                    location.height * this.cellHeight);
};

Grid.prototype.place = function(win, location) {
  var rect = this.rect(location);

  if (this.margin !== 0)
    rect = NSInsetRect(rect, this.margin, this.margin);

  rect = NSIntegralRect(rect);
  [win setFrame: rect];
};

// The position of a window inside that grid
Grid.Location = function(grid, column, row, width, height) {
  this.grid   = grid;
  this.column = column;
  this.row    = row;
  this.width  = width;
  this.height = height;
};

Grid.Location.prototype.fillColumn = function() {
  var result = _.clone(this);
  result.row = 0;
  result.height = this.grid.rows;
  return result;
};

Grid.Location.prototype.inc = function(widthOrHeight, n) {
  var result = _.clone(this);
  if (widthOrHeight === 'width') {
    result.width = Math.min(this.grid.columns - this.column, Math.max(this.width + n, 1));
  } else if (widthOrHeight === 'height') {
    result.height = Math.min(this.grid.rows - this.row, Math.max(this.height + n, 1));
  }

  return result;
};

Grid.Location.prototype.move = function(columnOrRow, n) {
  var result = _.clone(this),
      grid   = this.grid;

  if (columnOrRow === 'column') {
    result.column = Math.min(grid.columns - 1, Math.max(result.column + n, 0));
  } else if (columnOrRow === 'row') {
    result.row = Math.min(grid.rows - 1, Math.max(result.row + n, 0));
  }

  return result.constrainToScreen();
};

Grid.Location.prototype.constrainToScreen = function() {
  var result = _.clone(this),
      grid   = this.grid;

  if (result.column + result.width > grid.columns)
    result.width = grid.columns - result.column;

  if (result.row + result.height > grid.rows)
    result.height = grid.rows - result.row;

  return result;
};

Grid.Location.prototype.toString = function() {
  var s = "[loc (" + this.column + "," + this.row + ") [" + this.width + "x" + this.height + "]]";
  return s;
};


// Prettier syntax
var bind = function(modifiers, key, callback) {
  [Keys bind:key modifiers:modifiers fn:callback];
};

// Most of my bindings operate on a single modifier combination.
var bindings = function(modifiers, keys) {
  _.each(keys, function(callback, key) {
    bind(modifiers, key, callback);
  });
};

// Calls callback with the focused window, if there is one.
var focused = function(callback) {
  return function() {
    var win = [Win focusedWindow];
    if (typeof win !== 'undefined')
      callback.call(this, win);
  };
};

// If a window is focused, calls callback with the window's
// location within the given grid.
var onGrid = function(grid, callback) {
  var _grid = grid;

  return focused(function(win) {
    var screen = [win screen],
        grid   = _grid.onScreen(screen);
    callback.call(this, grid, win, grid.locate(win), screen);
  });
};

// Wrapper around onGrid that expects the callback to return a new
// grid location at which the focused window should be placed.
var relocate = function(grid, callback) {
  if (typeof callback === 'undefined') {
    callback = grid;
    grid = defaultGrid;
  }

  return onGrid(grid, function(grid, win, location, screen) {
    var destination = callback.call(this, location);
    grid.place(win, destination);
  });
};

// Actual config begins here ...
var defaultGrid = new Grid(3, 2, 1);
var combos = {
  mash:  ["CTRL", "ALT", "CMD"],
  cmdFn: ["CMD", "FN"],
  cmdFnStandIn: ["CTRL", "ALT"]
};

bindings(combos.mash, {
  // Reload
  'R': function() { [App reloadConfig]; },

  // Maximize
  'F': focused(function(win) { [win maximize]; }),

  // Fill column
  'U': relocate(function(loc) { return loc.fillColumn(); }),

  // Move window in grid
  'H': relocate(function(loc) { return loc.move('column', -1); }),
  'J': relocate(function(loc) { return loc.move('row', 1); }),
  'K': relocate(function(loc) { return loc.move('row', -1); }),
  'L': relocate(function(loc) { return loc.move('column', 1); }),

  // Grow/shrink column width
  'I': relocate(function(loc) { return loc.inc('width', -1); }),
  'O': relocate(function(loc) { return loc.inc('width', 1); })

  // previous / next screen
  // TODO restore these when I get home; only one monitor atm.
  // '1': focused(function(win, grid, screen) {
  //   moveToGridPropsOnScreen(win, [screen previousScreen], grid);
  // }),

  // '2': focused(function(win, grid, screen) {
  //   moveToGridPropsOnScreen(win, [screen nextScreen], grid);
  // })
});

// bindings(combos.cmdFn, { // TODO binding to FN currently swallows FN-less key presses
bindings(combos.cmdFnStandIn, {
  'H': focused(function(win) { [win focusWindowLeft]; }),
  'J': focused(function(win) { [win focusWindowDown]; }),
  'K': focused(function(win) { [win focusWindowUp]; }),
  'L': focused(function(win) { [win focusWindowRight]; })
});
