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
