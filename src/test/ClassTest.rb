require File.expand_path('simple', File.dirname(__FILE__))

# Trac ticket #206 fix
class Foo
  @bar = false
  class << self
    attr_accessor :bar
  end
end

test(Foo.bar, false, 'A')

# Test that allocate works.  From PickAxe
class MyClass
  attr_reader :a, :b, :c
  def MyClass.another_new(*args)
    o = allocate
    o.send(:initialize, *args)
    o
  end
  def initialize(a,b,c)
    @a, @b, @c = a, b, c
  end
end
mc = MyClass.another_new(4,5,6)
test(mc.a, 4, 'alloc a')
test(mc.b, 5, 'alloc b')
test(mc.c, 6, 'alloc c')


report
true
