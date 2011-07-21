require File.expand_path('simple', File.dirname(__FILE__))

class C
  def initialize(c)
    @c = c
  end
  def c_square(n)
    @c * n * n
  end
end
c = C.new(1)
m = c.method(:c_square)
p = m.to_proc

c2 = C.new(2)
m2 = c2.method(:c_square)
p2 = m2.to_proc

test(m.call(9),   81, 'm.call(9)')
test(m2.call(9), 162, 'm2.call(9)')

test(p.call(7),   49, 'p.call(7)')
test(p2.call(7),  98, 'p2.call(7)')


# There was a bug where setting the protection on a method, and if that
# method had the generic error message bridges, then it would change the
# error protection on the generic bridge, thus changing the protection for
# all methods in all classes that shared that bridge...
class C27
  def match(x) # Is, and should remain, public
  end

  def foo(x) # Is public now...
  end

  private :foo  # but now is private (test that match is still public)
end

test(C27.new.respond_to?(:match), true, "Method protection A")
test(C27.new.respond_to?(:foo), false,  "Method protection B")
test(C27.new.respond_to?(:foo, true), true,  "Method protection C")
test(nil.respond_to?(:call), false, "Trac 930 fails");

report

