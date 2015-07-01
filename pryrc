# -*- mode: ruby -*-
if ENV['TERM'] == 'dumb'
  Pry.config.pager  = false
  Pry.config.editor = 'emacsclient'
else
  Pry.config.editor = 'atom'
end

if defined?(PryByebug) || defined?(PryDebugger)
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
end

require 'date'
require 'bigdecimal'

class Date
  def inspect
    "#<Date: #{self}>"
  end
end

class BigDecimal
  def inspect
    "#{to_s('F')}bd"
  end
end

# cuz instance_variable_get() et al are too fkn long
class Object
  def ivs
    instance_variables
  end

  def iv(name)
    name = "@#{name}" unless name.to_s.start_with?("@")
    instance_variable_get(name)
  end

  def ivset(name, val)
    name = "@#{name}" unless name.to_s.start_with?("@")
    instance_variable_set(name, val)
  end
end
