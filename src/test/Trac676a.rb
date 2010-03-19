# simplified test for Trac 676

class A
  def ma
    $A << 100
  end
end

class B < A
  def ma
    $A << 200
    super()
  end
end

class C < B
  alias_method :get, :ma
end

$A = []
B.new.ma
bx = $A
$A = []
C.new.get
cx = $A
puts bx.inspect
puts cx.inspect
unless bx == [200, 100] ; raise 'error'; end
unless cx == [200, 100] ; raise 'error'; end

true
