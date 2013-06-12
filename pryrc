# -*- mode: ruby -*-
if ENV['TERM'] == 'dumb'
  Pry.config.pager  = false
  Pry.config.editor = 'emacsclient'
end
