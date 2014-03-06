/*global Window, api */

var mash = ['ctrl', 'cmd', 'shift'],
    malt = ['ctrl', 'alt', 'shift'];

function withFocused(cb) {
  var win = Window.focusedWindow();

  if (!win)
    return undefined;

  return cb(win, win.frame(), win.screen());
}

function focus(dir) {
  return function() {
    var win = Window.focusedWindow();
    if (win)
      win['focusWindow' + dir]();
    else
      Window.visibleWindowsMostRecentFirst()[0].focusWindow();
  };
}

function push(dir) {
  return function() {
    withFocused(function(win, frame, screen) {
      var screenFrame = screen.frameWithoutDockOrMenu();

      if (dir === 'Left')
        win.setTopLeft({ x: screenFrame.x, y: frame.y });
      else if (dir === 'Right')
        win.setTopLeft({ x: screenFrame.x + Math.round(screenFrame.width / 2), y: frame.y });
      else if (dir === 'Up')
        win.setTopLeft({ x: frame.x, y: screenFrame.y });
      else if (dir === 'Down')
        win.setTopLeft({ x: frame.x, y: screenFrame.y + Math.round(screenFrame.width / 2) });
    });
  };
}

function screen(which) {
  return function() {
    return withFocused(function(win, frame, screen) {
      var oldScreenRect = screen.frameWithoutDockOrMenu(),
          target        = screen[which + 'Screen'](),
          newScreenRect = target.frameWithoutDockOrMenu();

      var xRatio = newScreenRect.width / oldScreenRect.width,
          yRatio = newScreenRect.height / oldScreenRect.height;

      win.setFrame({
        x: (Math.round(frame.x - oldScreenRect.x) * xRatio) + newScreenRect.x,
        y: (Math.round(frame.y - oldScreenRect.y) * yRatio) + newScreenRect.y,
        width: Math.round(frame.width * xRatio),
        height: Math.round(frame.height * yRatio)
      });
    });
  };
}

function toGrid(x, y, width, height) {
  return function() {
    withFocused(function(win) {
      win.toGrid(x, y, width, height);
    });
  };
}

api.bind('h', mash, focus('Left'));
api.bind('j', mash, focus('Down'));
api.bind('k', mash, focus('Up'));
api.bind('l', mash, focus('Right'));

api.bind('h', malt, push('Left'));
api.bind('j', malt, push('Down'));
api.bind('k', malt, push('Up'));
api.bind('l', malt, push('Right'));

api.bind('b', mash, screen('previous'));
api.bind('n', mash, screen('next'));

api.bind('m',     mash, toGrid(  0,   0,   1,   1));
api.bind('left',  mash, toGrid(  0,   0, 0.5,   1));
api.bind('right', mash, toGrid(0.5,   0, 0.5,   1));
api.bind('up',    mash, toGrid(  0,   0,   1, 0.5));
api.bind('down',  mash, toGrid(  0, 0.5,   1, 0.5));

/**
 * Ganked from https://github.com/carlo/bash-it/blob/master/dotfiles/.phoenix.js
 * <3
 */

// #### Window#toGrid()
//
// This method can be used to push a window to a certain position and size on
// the screen by using four floats instead of pixel sizes.  Examples:
//
//     // Window position: top-left; width: 25%, height: 50%
//     someWindow.toGrid( 0, 0, 0.25, 0.5 );
//
//     // Window position: 30% top, 20% left; width: 50%, height: 35%
//     someWindow.toGrid( 0.3, 0.2, 0.5, 0.35 );
//
// The window will be automatically focussed.  Returns the window instance.
Window.prototype.toGrid = function( x, y, width, height ) {
  var screen = this.screen().frameWithoutDockOrMenu(),
      padding = 2;

  this.setFrame({
    x: Math.round( x * screen.width ) + padding + screen.x,
    y: Math.round( y * screen.height ) + padding + screen.y,
    width: Math.round( width * screen.width ) - ( 2 * padding ),
    height: Math.round( height * screen.height ) - ( 2 * padding )
  });

  this.focusWindow();

  return this;
};
