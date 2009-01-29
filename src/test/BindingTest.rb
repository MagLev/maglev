class TB
  def test(arg1, *arg2, &blk)
    @iva = 99
    b = nil
    ta = arg1
    arg2.each do | ba1 |
       tba1 = ba1
       ba1.each do | ba2 |
         tba2 = ba2
         b = binding 
       end
    end
    b
  end

  def test_noargs
    wwv = 'radio'
    binding
  end
end

tobj = TB.new
b = tobj.test(66, *[ [ 77 ] ] ) { |ca1| ca1 + 1000 }
d = eval('[ arg1, arg2, blk,   @iva, ta,   ba1, tba1, ba2, tba2 ] ', b)
dx = d.dup
dx[2] = dx[2].class.name
unless dx = [ 66 , [[77]], ExecBlock, 99, 66, [77], [77], 77, 77] ; raise 'error'; end

# test a creating binding at main program level
TBINDSTR = String.new
[ 'abc', 'wvt' ].each do |str |
  eval(<<-EOS, binding )
    TBINDSTR.concat("_#{str}")
  EOS
end

unless TBINDSTR == '_abc_wvt' ; raise 'error'; end

# Binding created by a zero-arg method
bz = tobj.test_noargs
bv = eval( ' wwv ' , bz )
unless bv == 'radio' ; raise 'error'; end

puts "==== BindingTest ok"

true


