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


report

true
