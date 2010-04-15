# AModule.instance_methods was returning methods from Module, which should not happen.
# Plus, it seems, the protection field isn't right re protected.
require File.expand_path('simple', File.dirname(__FILE__))

m = Module.new

test(m.instance_methods,           [], "Empty Module instance_methods")
test(m.public_instance_methods,    [], "Empty Module public_instance_methods")
test(m.protected_instance_methods, [], "Empty Module protected_instance_methods")
test(m.private_instance_methods,   [], "Empty Module private_instance_methods")


module AModule
  def foo ; 10; end

  private
  def bar ; 20; end

  protected
  def quux ; 30; end
end

test(AModule.instance_methods.sort,           ['foo', 'quux'], "AModule instance_methods")
test(AModule.public_instance_methods.sort,    ['foo'], "AModule public_instance_methods")
test(AModule.protected_instance_methods.sort, ['quux'], "AModule protected_instance_methods")
test(AModule.private_instance_methods.sort,   ['bar'], "AModule private_instance_methods")



c = Class.new

result = c.instance_methods
test(result.size > 30, true, "Empty Class instance_methods")

result = c.public_instance_methods
test(result.size > 30, true, "Empty Class public_instance_methods")

result = c.protected_instance_methods
test(result.size, 0, "Empty Class protected_instance_methods")

result = c.private_instance_methods
test(result.size > 60, true, "Empty Class private_instance_methods")



class AClass
  def foo ; 10; end

  private
  def bar ; 20; end

  protected
  def quux ; 30; end
end

result = AClass.instance_methods
test(result.size > 30, true, "AClass instance_methods")

result = AClass.public_instance_methods
test(result.size > 30, true, "AClass public_instance_methods")

result = AClass.protected_instance_methods
test(result.size, 1, "AClass protected_instance_methods")

result = AClass.private_instance_methods
test(result.size > 60, true, "AClass private_instance_methods")

report
