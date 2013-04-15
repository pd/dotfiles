// bindings for https://github.com/sdegutis/Windows

var combos = {
  mash:  ["CTRL", "ALT", "CMD"],
  cmdFn: ["CMD", "FN"],
  cmdFnStandIn: ["CTRL", "ALT"]
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

// If there is currently a window focused, call callback with:
// * The window
// * The result of gridProps(window)
// * The window's screen
var focused = function(callback) {
  return function() {
    var win = [Win focusedWindow];
    if (typeof win !== 'undefined')
      callback.apply(this, [win, gridProps(win), [win screen]]);
  };
};

// Wrap a call to focus that passes the window's gridProps to
// the given callback, which should modify the grid object to
// specify the window's desired new location.
var moveTo = function(modifyGrid) {
  return focused(function(win, grid, screen) {
    modifyGrid.call(this, grid);
    moveToGridProps(win, grid);
  });
};

bindings(combos.mash, {
  // Reload
  'R': function() { [App reloadConfig]; },

  // Maximize
  'F': focused(function(win) { [win maximize]; });

  // Fill column
  'U': moveTo(function(grid) {
    grid.origin.y = 0;
    grid.size.height = 2;
  }),

  // Move window in grid
  'H': moveTo(function(grid) {
    grid.origin.x = Math.max(grid.origin.x - 1, 0);
  }),

  'J': moveTo(function(grid) {
    grid.origin.y = 1;
    grid.size.height = 1;
  }),

  'K': moveTo(function(grid) {
    grid.origin.y = 0;
    grid.size.height = 1;
  }),

  'L': moveTo(function(grid) {
    grid.origin.x = Math.min(grid.origin.x + 1, 3 - grid.size.width);
  }),

  // Grow/shrink column width
  'I': moveTo(function(grid) {
    grid.size.width = Math.max(grid.size.width - 1, 1);
  }),

  'O': moveTo(function(grid) {
    grid.size.width = Math.min(grid.size.width + 1, 3 - grid.origin.x);
  }),

  // previous / next screen
  '1': focused(function(win, grid, screen) {
    moveToGridPropsOnScreen(win, [screen previousScreen], grid);
  }),

  '2': focused(function(win, grid, screen) {
    moveToGridPropsOnScreen(win, [screen nextScreen], grid);
  })
});

// bindings(combos.cmdFn, { // TODO binding to FN currently swallows FN-less key presses
bindings(combos.cmdFnStandIn, {
  'H': focused(function(win) { [win focusWindowLeft]; }),
  'J': focused(function(win) { [win focusWindowDown]; }),
  'K': focused(function(win) { [win focusWindowUp]; }),
  'L': focused(function(win) { [win focusWindowRight]; })
});

// helper functions
var gridProps = function(win) {
    var winFrame = [win frame];
    var screenRect = [[win screen] frameWithoutDockOrMenu];

    var thirdScrenWidth = screenRect.size.width / 3.0;
    var halfScreenHeight = screenRect.size.height / 2.0;

    return CGRectMake(Math.round((winFrame.origin.x - NSMinX(screenRect)) / thirdScrenWidth),
                      Math.round((winFrame.origin.y - NSMinY(screenRect)) / halfScreenHeight),
                      Math.max(Math.round(winFrame.size.width / thirdScrenWidth), 1),
                      Math.max(Math.round(winFrame.size.height / halfScreenHeight), 1));
};

var moveToGridProps = function(win, gridProps) {
  moveToGridPropsOnScreen(win, [win screen], gridProps);
}

var moveToGridPropsOnScreen = function(win, screen, gridProps) {
    var screenRect = [screen frameWithoutDockOrMenu];

    var thirdScrenWidth = screenRect.size.width / 3.0;
    var halfScreenHeight = screenRect.size.height / 2.0;

    var newFrame = CGRectMake((gridProps.origin.x * thirdScrenWidth) + NSMinX(screenRect),
                              (gridProps.origin.y * halfScreenHeight) + NSMinY(screenRect),
                              gridProps.size.width * thirdScrenWidth,
                              gridProps.size.height * halfScreenHeight);

    newFrame = NSInsetRect(newFrame, 5, 5); // acts as a little margin between windows, to give shadows some breathing room
    newFrame = NSIntegralRect(newFrame);

    [win setFrame: newFrame];
};
