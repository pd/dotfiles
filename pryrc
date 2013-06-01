# -*- mode: ruby -*-
if ENV['TERM'] == 'dumb'
  Pry.config.pager  = false
  Pry.config.editor = 'emacsclient'
end

Pry.config.prompt = proc do |target_self, nesting, _|
  "#{Pry.view_clip(target_self)}> "
end
