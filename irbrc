# -*- mode: ruby -*-
require 'pp'
require 'irb/completion'

begin
  require 'pry'
rescue LoadError
end

if ENV['EMACS'] || ENV['INSIDE_EMACS']
  if defined?(Pry)
    Pry.config.pager = false
  end
end

if defined?(Pry)
  pry
  exit
end
