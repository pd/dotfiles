// bindings for https://github.com/sdegutis/Windows

var mash  = ["CMD", "ALT", "CTRL"];

// mash-R reloads this config for testing
[Keys bind:"R" modifiers:mash fn: function() {
    [App reloadConfig];
}];

// mash-F: fill screen
[Keys bind:"F" modifiers:mash fn: function() {
    [[Win focusedWindow] maximize];
}];

// mash-U: fill column
[Keys bind:"U" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    var r = gridProps(win);
    r.origin.y = 0;
    r.size.height = 2;
    moveToGridProps(win, r);
}];

// mash-[HJKL]: move
[Keys bind:"H" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    var r = gridProps(win);
    r.origin.x = Math.max(r.origin.x - 1, 0);
    moveToGridProps(win, r);
}];

[Keys bind:"J" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    var r = gridProps(win);
    r.origin.y = 1;
    r.size.height = 1;
    moveToGridProps(win, r);
}];

[Keys bind:"K" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    var r = gridProps(win);
    r.origin.y = 0;
    r.size.height = 1;
    moveToGridProps(win, r);
}];

[Keys bind:"L" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    var r = gridProps(win);
    r.origin.x = Math.min(r.origin.x + 1, 3 - r.size.width);
    moveToGridProps(win, r);
}];

// mash-[IO]: shrink from right, grow from right
[Keys bind:"I" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    var r = gridProps(win);
    r.size.width = Math.max(r.size.width - 1, 1);
    moveToGridProps(win, r);
}];

[Keys bind:"O" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    var r = gridProps(win);
    r.size.width = Math.min(r.size.width + 1, 3 - r.origin.x);
    moveToGridProps(win, r);
}];

// cmd-fn-[HJKL]: focus window in direction
[Keys bind:"H" modifiers:cmdFn fn: function() {
    var win = [Win focusedWindow];
    if (!win) return;
    [win focusWindowLeft];
}];

[Keys bind:"J" modifiers:cmdFn fn: function() {
    var win = [Win focusedWindow];
    if (!win) return;
    [win focusWindowDown];
}];

[Keys bind:"K" modifiers:cmdFn fn: function() {
    var win = [Win focusedWindow];
    if (!win) return;
    [win focusWindowUp];
}];

[Keys bind:"L" modifiers:cmdFn fn: function() {
    var win = [Win focusedWindow];
    if (!win) return;
    [win focusWindowRight];
}];

// mash-[12]: prev/next screen
[Keys bind:"1" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    moveToGridPropsOnScreen(win, [[win screen] previousScreen], gridProps(win));
}];

[Keys bind:"2" modifiers:mash fn: function() {
    var win = [Win focusedWindow];
    moveToGridPropsOnScreen(win, [[win screen] nextScreen], gridProps(win));
}];


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
