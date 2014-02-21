/*global Window, api */

var mash = ['ctrl', 'cmd', 'shift'],
    malt = ['ctrl', 'alt', 'shift'];

function withFocused(cb) {
  var win    = Window.focusedWindow(),
      frame  = win.frame(),
      screen = win.screen();

  return cb(Window.focusedWindow(), frame, screen);
}

function focus(dir) {
  return function() {
    withFocused(function(win) { win['focusWindow' + dir](); });
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

function fullscreen() {
  withFocused(function(win, frame, screen) {
    win.setFrame(screen.frameWithoutDockOrMenu());
  });
}

function halfwidth() {
  withFocused(function(win, frame, screen) {
    var screenFrame = screen.frameWithoutDockOrMenu();

    win.setFrame({
      x: frame.x,
      y: frame.y,
      width: Math.round(screenFrame.width / 2),
      height: frame.height
    });
  });
}

function halfheight() {
  withFocused(function(win, frame, screen) {
    var screenFrame = screen.frameWithoutDockOrMenu();

    win.setFrame({
      x: frame.x,
      y: frame.y,
      width: frame.width,
      height: Math.round(screenFrame.height / 2)
    });
  });
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

api.bind('m', mash, fullscreen);
api.bind('/', mash, halfwidth);
api.bind('.', mash, halfheight);
