# This test case from Haml.  The condensed haml code is:
#
# module Haml
#   class Engine
#     def render(scope = Object.new)
#       scope_object = scope
#       scope = scope_object.instance_eval{binding}
#       set_locals({:_hamlout => "buffer", :_erbout => "erbout"},
#                  scope, scope_object)
#       puts eval("_hamlout", scope)
#     end
#     def set_locals(locals, scope, scope_object)
#       scope_object.send(:instance_variable_set, '@_haml_locals', locals)
#       set_locals = locals.keys.map { |k| "#{k} = @_haml_locals[#{k.inspect}]" }.join("\n")
#       puts "---------- EVAL STRING: ----------"
#       puts set_locals
#       puts "----------------------------------"
#       eval(set_locals, scope)
#     end
#   end
# end

# e = Haml::Engine.new
# e.render

# This test case creates a binding with a local variable named "_hamlout",
# but the eval does not have access to it.
class C
 def ma
   scope = Object.new.instance_eval { binding }
   puts "AA -------------------"
   eval('_hamlout = "FOO" ', scope)
   puts "BB -------------------"
   ax = eval("_hamlout", scope)     # => prints "FOO" in MRI
   puts ax
   unless ax = 'FOO' ; raise 'error'; end
 end
end
C.new.ma
true
#################### Trac Info
# ID:         641
# Summary:    eval does not have access to local variables
# Changetime: 2010-01-06 01:01:22+00:00
###

#  The following code prints "FOO" in MRI, but raises an exception in MagLev:
#  
#  
#  {{{
#  # This test case creates a binding with a local variable named "_hamlout",
#  # but the eval does not have access to it.
#  scope = Object.new.instance_eval { binding }
#  eval('_hamlout = "FOO"', scope)
#  puts eval("_hamlout", scope)     # => prints "FOO" in MRI
#  }}}
#  
#  The error:
#  
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb
#  error , NoMethodError: undefined method `_hamlout' for Object,
#            during /Users/pmclain/GemStone/checkouts/git/src/test/TracXXX.rb
#  ERROR 2010, NoMethodError: undefined method `_hamlout' for Object (NoMethodError)
#  
#  
#  }}}
#  
#  