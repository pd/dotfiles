require 'pp'
require 'readline'
require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:AUTO_INDENT] = true

def commify(number)
  number.to_s.reverse.scan(/.{1,3}/).reverse.map(&:reverse).join ','
end
