require File.expand_path('simple', File.dirname(__FILE__))


#####################################################
class A
  def foo(ary); ary << :A; end
end
class B < A
  def foo(ary); ary << :B; super; end
end
class C < B
  def foo(ary); ary << :C; super; end
end

module L1
  def foo(ary); ary << :L1; super; end
end
module L2
  include L1
  def foo(ary); ary << :L2; super; end
end

module L3
  include L2
  def foo(ary)
    ary << :L3
    super;
  end
end

class C; include L3; end
class B; include L3; end

c = C.new
l1 = L1
l2 = L2
l3 = L3

#  this case gets infinite recursion still
# test(c.foo([]), [:C, :L3, :L2, :L1, :B, :L3, :L2, :L1, :A], 'Test one')

b = B.new

test(b.foo([]), [:B, :L3, :L2, :L1, :A], 'Test two')



# ========================================

module M1
  def foo
    'M1#foo'
  end
end
module M2
  def foo
    'M2#foo'
  end
end

class C1
  include( M2, M1)
  def foo
    'C1#foo'
  end
end

class C2
  include M1
end

class C3
  include(M2, M1)
end
class C4
  # opposite order from C3
  include(M1, M2)
end
class C5
  extend M1
end
class C6
  extend M1
  extend M2
end
class C7
  extend M2
  extend M1
end

test(C1.new.foo, 'C1#foo', 'C1#foo')
test(C2.new.foo, 'M1#foo', 'C2#foo')
test(C3.new.foo, 'M2#foo', 'C3#foo')
test(C4.new.foo, 'M1#foo', 'C4#foo')
test(C5.foo,     'M1#foo', 'C5#foo')
test(C6.foo,     'M2#foo', 'C6#foo')
test(C7.foo,     'M1#foo', 'C7#foo')

class Cl319A
  def initialize(enum = nil, &block)
    @iva = enum
  end
  def iva
    @iva
  end
end
class Cl319B < Cl319A
  def initialize(*args, &block)
    super
  end
end

o = Cl319B.new([95])
test(o.iva, [95], "ticket 319 a");
o = Cl319B.new([101,33])
test(o.iva, [101,33], "ticket 319 b");

report

true
