# -*- mode: ruby -*-
if ENV['TERM'] == 'dumb'
  Pry.config.pager  = false
  Pry.config.editor = 'emacsclient'
end

Pry.config.prompt = proc do |target_self, nesting, _|
  "#{Pry.view_clip(target_self)}> "
end

if defined?(Rails) && Rails.env
  begin
    extend Rails::ConsoleMethods
  rescue NameError
    puts "Could not extend Rails::ConsoleMethods"
  end

  begin
    require 'pry-rails'
  rescue LoadError
  end
end
