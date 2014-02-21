var mash = ['ctrl', 'cmd', 'shift'],
    malt = ['ctrl', 'alt', 'shift'];

function focus(dir) {
  return function() {
    var win = Window.focusedWindow();
    win['focusWindow' + dir]();
  };
}

function push(dir) {
  return function() {
    var win = Window.focusedWindow(),
        winFrame    = win.frame(),
        screenFrame = win.screen().frameWithoutDockOrMenu();

    if (dir === 'Left')
      win.setTopLeft({ x: screenFrame.x, y: winFrame.y });
    else if (dir === 'Right')
      win.setTopLeft({ x: screenFrame.x + Math.round(screenFrame.width / 2), y: winFrame.y });
    else if (dir === 'Up')
      win.setTopLeft({ x: winFrame.x, y: screenFrame.y });
    else if (dir === 'Down')
      win.setTopLeft({ x: winFrame.x, y: screenFrame.y + Math.round(screenFrame.width / 2) });
  };
}

function screen(which) {
  return function() {
    var win    = Window.focusedWindow(),
        frame  = win.frame(),
        screen = win.screen()[which + 'Screen']();

    var oldScreenRect = win.screen().frameWithoutDockOrMenu();
    var newScreenRect = screen.frameWithoutDockOrMenu();

    var xRatio = newScreenRect.width / oldScreenRect.width;
    var yRatio = newScreenRect.height / oldScreenRect.height;

    win.setFrame({
      x: (Math.round(frame.x - oldScreenRect.x) * xRatio) + newScreenRect.x,
      y: (Math.round(frame.y - oldScreenRect.y) * yRatio) + newScreenRect.y,
      width: Math.round(frame.width * xRatio),
      height: Math.round(frame.height * yRatio)
    });
  };
}

function fullscreen() {
  var win = Window.focusedWindow();
  win.setFrame(win.screen().frameWithoutDockOrMenu());
}

function halfwidth() {
  var win = Window.focusedWindow(),
      winFrame    = win.frame(),
      screenFrame = win.screen().frameWithoutDockOrMenu();

  win.setFrame({
    x: winFrame.x,
    y: winFrame.y,
    width: Math.round(screenFrame.width / 2),
    height: winFrame.height
  });
}

function halfheight() {
  var win = Window.focusedWindow(),
      winFrame    = win.frame(),
      screenFrame = win.screen().frameWithoutDockOrMenu();

  win.setFrame({
    x: winFrame.x,
    y: winFrame.y,
    width: winFrame.height,
    height: Math.round(screenFrame.height / 2),
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
