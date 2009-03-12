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


# Test that the inherited() callback works.
#
# There are two paths that we have to test:
# 1: RubyCompiler>>defineClassNamed:rubyMethod:inScope:superclass:, for the
#       class Foo < Bar; ... ;end case
# 2: Class.new in Class.rb
$child_class = nil

# Using Class.new syntax
top = Class.new do
  def self.inherited(cls)
    $child_class = cls
  end
end

test($child_class, nil, "inherited A")
child = Class.new(top)
test($child_class, child, "inherited B")

# Using class F < B syntax
class AnotherTop < top
end
test($child_class, AnotherTop, "inherited C")

# Test that the proper version of Class.new is called
class C
  attr_reader :x
  def initialize(&b)
    raise "NO BLOCK PASSED" unless block_given?
    @x = 10
  end
end

c = C.new { true }
test(c.x, 10, "c.x")

class D
  attr_reader :x
  def initialize(x)
    raise "BLOCK PASSED" if block_given?
    @x = x
  end
end

d = D.new(1)
test(d.x, 1, "d.x")

report
true
