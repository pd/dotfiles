require 'rubygems'
require 'logger'
require 'pp'
require 'readline'
require 'irb/completion'
require 'irb/ext/save-history'
require 'wirble'

IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:AUTO_INDENT] = true

Wirble.init

def commify(number)
  number.to_s.reverse.scan(/.{1,3}/).reverse.map(&:reverse).join ','
end

if ENV.include?('RAILS_ENV')
  def incfr
    include FixtureReplacement
  end

  unless Object.const_defined?('RAILS_DEFAULT_LOGGER')
    Object.const_set('RAILS_DEFAULT_LOGGER', Logger.new(STDOUT))
  end
end
