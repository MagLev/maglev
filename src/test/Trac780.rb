# Proc#binding is not implemented.  The error message is confusing.
class C
  def quick_emit(&block)
    b = block.binding
    x = eval('abc', b)
    raise "Fail" unless x == 123
  end
end

abc = 123
C.new.quick_emit do
  abc = 10
end
