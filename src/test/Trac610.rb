
class A
  eval "class B; end"
  def c
    eval "class C; $w = self; end;"
  end
end

A.new.c
bx = A::B
cx = $w
unless (bb = bx.inspect) == 'A::B' ; raise 'err1'; end
unless (cc = cx.inspect) == 'A::C' ; raise 'err1'; end 
true
