
class ClsA
  def foo(a=b=9)
      puts a
      puts b
      aa = a
      bb = b
      [a,b]
      # nil.pause
  end
end
o = ClsA.new
raise Exception unless o.foo(3) == [3,nil]
raise Exception unless o.foo() == [9,9]
