
# Test class creation via metaprogramming

Klass = Class.new(Object)
c1 = Klass.new   # Fails


klass = Class.new(Object)
c = klass.new    # Fails

# Once the above code is working, make sure the following also works:
#
# klass = Class.new(Object) do
#   attr_accessor :foo
#   def bar
#     puts "BAR"
#   end
# end

# c = klass.new
# p c.bar

# c.foo = "foo"
# p c.foo
