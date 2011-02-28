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
