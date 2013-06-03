## Helpers

# Each desktop is split 24 ways when it comes to resizing
# windows.
scaleSteps = 12

# Helper to move the window on the screen.
moveWindow = (fn) ->
    win = api.focusedWindow()
    frame = win.frame()
    fn frame if fn
    win.setFrame frame

# Coffeescript is retarded. write this hash map helper.
mapObj = (obj, fn) ->
    cb = (m, v, k) ->
        m[k] = fn(v, k)
        return m
     _.reduce obj, cb, {}

# Abstract out some differences between X and Y coordinates.
axisMap =
    x:
        field: 'width'
        NSMax: NSMaxX
        NSMin: NSMinX
        compass: ['east', 'west']
        directions: ['left', 'right']
    y:
        field: 'height'
        NSMax: NSMaxY
        NSMin: NSMinY
        compass: ['north', 'south']
        directions: ['left', 'north']

# Generate a set of coordinates on the screen
# that a window can be mounted onto.
Screen::anchorPoints = ->
    rect = @frameIncludingDockAndMenu()
    rectMenu = @frameWithoutDockOrMenu()

    min = x: NSMinX(rect), y: NSMinY(rect)
    max = x: NSMaxX(rect), y: NSMaxY(rect)

    step =
        x: Math.round (max.x or min.x) / 2
        y: Math.round (max.y or min.y) / 2

    result =
        x: i for i in [0..(max.x or min.x)] by step.x
        y: i for i in [0..(max.y or min.x)] by step.y

    result.x.reverse() if step.x < 0
    result.y.reverse() if step.y < 0

    [result.x[0], result.x[-1]] = [NSMinX(rectMenu), NSMaxX(rectMenu)]
    [result.y[0], result.y[-1]] = [NSMinY(rectMenu), NSMaxY(rectMenu)]

    return result

# Generate a list of anchors on a window, to be
# mounted onto the desktop.
Window::anchors = ->
    rect =  @frame()
    x: [1, rect.size.width / 2, rect.size.width]
    y: [1, rect.size.height / 2, rect.size.height]

# Return the current coordinates for each of the
# window's anchors, in relation to the position
# on the desktop.
Window::anchorCoords = ->
    addOrigin = (offset) -> (v) -> v + offset
    frame = @frame()

    mapObj @anchors(), (anchors, axis) ->
        _.map anchors, addOrigin(frame.origin[axis])

# Find the anchor point on the screen closest to
# the relevant anchor on the window.
Window::closestAnchorPoint = ->
    normalize = (points) ->
        grid = _.map points.x, (xv, xk) ->
            _.map points.y, (yv, yk) ->
                { xk: xk, yk: yk, xv: xv, yv: yv }
        # Flatten it out so there are x*y coordinates
        flat = _.flatten grid, true

        # We want the middle to be more 'sticky' than the rest?
        _.sortBy flat, (point) ->
            ((point.xk + 1) % 2) + ((point.yk + 1) % 2)

    # Generate a merged, normalized structure that
    # is more suitable to functional iteration,
    # and doesn't require us to access variables
    # outside of our functions.
    screen = normalize @screen().anchorPoints()
    window = normalize @anchorCoords()
    zipped = _.zip screen, window

    # We already built in a preference towards the middle
    # of the screen when we normalized the list, so we can
    # run a basic _.min here and get away with it.
    closest = _.min zipped, (anchor) ->
        dx = anchor[0].xv - anchor[1].xv
        dy = anchor[0].yv - anchor[1].yv

        # Apply our trusty Pythagorean theorem to determine
        # relative distances.
        distance = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))
        Math.round(distance)

    { x: closest[0].xk, y: closest[0].yk }

# Attach a window's anchor to the matching anchor
# point on the screen.
#
# The screen parameter is optional, and can be used to
# flip windows # between monitors.
Window::anchorToPoint = (xc, yc, screen) ->
    frame = @frame()
    points = (screen ? @screen()).anchorPoints()

    adjust =
        0: (i) -> i
        1: (i, k) -> i - Math.round(frame.size[k] / 2)
        2: (i, k) -> i - frame.size[k]

    moveWindow (frame) ->
        frame.origin.x = adjust[xc] points.x[xc], 'width'
        frame.origin.y = adjust[yc] points.y[yc], 'height'

# Attach a window to an adjacant anchor point, based
# it's current position.
Window::shiftAnchorPoint = (xdiff, ydiff, screen) ->
    # we actually want the closest matching point
    # on the current screen, regardless of
    # wether we are moving it to the other screen.
    closest = @closestAnchorPoint()

    adjust = (ind, diff) ->
        retval = ind + diff
        if retval in [0, 1, 2] then return retval else return ind

    @anchorToPoint(
        adjust(closest.x, xdiff),
        adjust(closest.y, ydiff),
        screen
    )

Window::scaleCardinal = (dir) ->
    screenFrame = @screen().frameWithoutDockOrMenu()

    points = @screen().anchorPoints()
    closest = @closestAnchorPoint()

    axis = if dir in ['east', 'west'] then 'x' else 'y'
    axisField = axisMap[axis].field
    axisMin = axisMap[axis].NSMin
    axisMax = axisMap[axis].NSMax

    scaleIncrement = Math.round(screenFrame.size[axisField] / scaleSteps)

    closestAxis = closest[axis]
    closestOnAxis = points[axis][closestAxis]


    # by default we will scale from the bottom right corner
    isGrowing = true if dir in ['east', 'south']

    # On the edges, we flip the directions to grow/shrink, to
    # avoid having to move the window away, to grow.
    isGrowing = !isGrowing if closestAxis == 2

    # The window will remain anchored to the point, and
    # will scale out linearly from the point.
    adjustOrigin = {
        0: (size, i) -> i
        1: (size, i) -> i - Math.round(size / 2)
        2: (size, i) -> i - size
    }[closestAxis]

    # Reset the initial size to the closest multiple
    # of the scale increment.
    adjustBaseSize = (size) ->
        Math.round(size / scaleIncrement) * scaleIncrement

    moveWindow (frame) ->
        # Determine new frame size after the scaling has been done
        size = adjustBaseSize frame.size[axisField]
        size += if (isGrowing) then scaleIncrement else -scaleIncrement

        # Determine new origin point on this axis.
        origin = adjustOrigin size, closestOnAxis

        # Make sure the window doesn't adjust too far so as to move out
        # of the screen.
        belowMax = origin + size - 5  <= axisMax screenFrame
        aboveMin = origin >= axisMin screenFrame
        if belowMax and aboveMin
            frame.size[axisField] = size
            frame.origin[axis] = origin
