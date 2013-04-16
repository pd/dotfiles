clipboardContents = ->
  board = NSPasteboard.generalPasteboard
  body  = board.stringForType_(NSPasteboardTypeString)
  body?.toString()

evalJs = (code) ->
  return unless code? && code != ''
  eval code

evalCoffee = (code) ->
  return unless code? && code != ''
  evalJs compile(code)
