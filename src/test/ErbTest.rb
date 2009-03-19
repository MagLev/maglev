require File.expand_path('simple', File.dirname(__FILE__))

require 'erb'

x = 42
template = ERB.new "The value of x is <%= x %>"
test(template.result(binding), "The value of x is 42", "ERB 1")

report
