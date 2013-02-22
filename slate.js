// mash aka ctrl,alt,cmd
var mash = function(key, fn) {
  slate.bind(key + ':ctrl,alt,cmd', fn);
}

// mash hjkl => that side of the screen
var push = {
  'up':    slate.operation('push', { 'direction': 'up',    'style': 'bar-resize:screenSizeY/2' }),
  'down':  slate.operation('push', { 'direction': 'down',  'style': 'bar-resize:screenSizeY/2' }),
  'left':  slate.operation('push', { 'direction': 'left',  'style': 'bar-resize:screenSizeX/2' }),
  'right': slate.operation('push', { 'direction': 'right', 'style': 'bar-resize:screenSizeX/2' })
};

mash('h', function(win) { win.doOperation(push.left); });
mash('j', function(win) { win.doOperation(push.down); });
mash('k', function(win) { win.doOperation(push.up); });
mash('l', function(win) { win.doOperation(push.right); });

// mash up/down/left/right => grow/shrink
var resize = function(win, direction) {
  var width  = direction == 'left' ? '-5%' : direction == 'right' ? '+5%' : '0';
  var height = direction == 'down' ? '-5%' : direction == 'up'    ? '+5%' : '0';
  var op = slate.operation('resize', { 'width': width, 'height': height });
  win.doOperation(op);
};

mash('up',    function(win) { resize(win, 'up'); });
mash('down',  function(win) { resize(win, 'down'); });
mash('left',  function(win) { resize(win, 'left'); });
mash('right', function(win) { resize(win, 'right'); });

// cmd+shift+space => focus window via hints
mash('space:cmd,shift', function(win) {
  var hint = slate.operation('hint', {
    'characters': 'ASDFQWERZXCV'
  });
  win.doOperation(hint);
});

// mash m => maximize
mash('m', function(win) {
  var maximize = slate.operation('move', {
    'x': 'screenOriginX',
    'y': 'screenOriginY',
    'width':  'screenSizeX',
    'height': 'screenSizeY'
  });
  win.doOperation(maximize);
});
