
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
#################### Trac Info
# ID:         232
# Summary:    optional method args , multiple default assign not working, bad ParseTree output
# Changetime: 2009-02-14 01:52:47+00:00
###

#  The example shown below is assigning to args in wrong order,
#  and if you call   foo()   we will incorrectly raise an
#  ArgumentError for too few args .  Need to try ParseTree v3 to 
#  see if it helps.   The sexpr we get now is pretty ugly for this case.
#  {{{
#  class ClsA
#      def foo(a=b=9)
#        nil.pause
#        puts a
#        puts b
#      end
#    end
#    o = ClsA.new
#    o.foo(3,4)
#  
#  ....
#  topaz 1> fr 5
#  5 ClsA >> foo::                                      (envId 1) @6 line 2   [methId 1004267777]
#      receiver [1004268289  ClsA]       aClsA
#      b 3
#      a 4
#  
#  }}}
#  