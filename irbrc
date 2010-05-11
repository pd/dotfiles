require 'pp'
require 'readline'

IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:AUTO_INDENT] = true

begin
  require 'bond'
  Bond.start
rescue LoadError
end
