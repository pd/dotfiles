# -*- mode: ruby -*-
require 'pp'
require 'readline'

IRB.conf[:AUTO_INDENT] = true

if defined?(::Bundler)
  if global_gems_path = Gem.path.find { |p| p =~ /@global/ }
    $LOAD_PATH.concat Dir.glob("#{global_gems_path}/gems/*/lib")
  end
end

begin
  require 'bond'
  Bond.start
rescue LoadError
  require 'irb/completion'
end

%w[pry awesome_print looksee pasteboard pbcopy].each do |lib|
  begin
    require lib
  rescue LoadError
    puts "Failed to load #{lib}"
  end
end

if ENV['EMACS'] || ENV['INSIDE_EMACS']
  if defined?(Pry)
    Pry.config.pager = false
  end
end
