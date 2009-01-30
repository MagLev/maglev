# From Haml's requires...

raise "ActionView should not be defined at top:" if defined?(ActionView)

if defined?(ActionView) # this should prevent the definition of the module
  module ActionView     # this is the culprit...
  end
end
