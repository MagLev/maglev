# From Haml's requires...

raise "ActionView should not be defined at top:" if defined?(ActionView)

x = 0
if defined?(ActionView) # this should prevent the definition of the module
  module ActionView     # this is the culprit...
  end
else
  x = 334
end
unless x == 334 ; raise 'error' ; end

puts "===Trac334.rb ok"

true
