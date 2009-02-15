
class ClsA
  def foo(a=b=9)
      puts a
      puts b
      aa = a
      bb = b
      # nil.pause
  end
end
o = ClsA.new
o.foo(3,4)
o.foo()
