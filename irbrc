require 'pp'
require 'readline'

IRB.conf[:AUTO_INDENT] = true

begin
  require 'bond'
  Bond.start
rescue LoadError
  require 'irb/completion'
end
