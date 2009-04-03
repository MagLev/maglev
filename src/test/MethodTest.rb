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

report

