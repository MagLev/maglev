# Should never print 'false'

$errcnt = 0
class C629
  def check_res(val, expval, name)
    unless expval == val
      puts "  FAIL, #{name} == #{val.inspect}, expected #{expval.inspect} "
      $errcnt = $errcnt + 1
#      nil.pause # uncomment to debug
    end
  end
 
  def ma
    # L: ary, S: nil, R: argscat
    puts "a, b, c = 1, *2"
    a, b, c = 1, *2
    check_res( a , 1, 'a');
    check_res( b , 2, 'b');
    check_res( c , nil, 'c');
  end
     
  def mb
    puts "a, b, c = 1, 2, *3"
    a, b, c = 1, 2, *3
    check_res( a , 1, 'a');
    check_res( b , 2, 'b');
    check_res( c , 3, 'c');
  end
     
  def mc
    # L: ary, S: x, R: argscat
    puts "a, *b = 1, *2"
    a, *b = 1, *2   # nonStarSz 1 , lsz 2, rsz 2 
    check_res( a , 1, 'a');
    check_res( b , [2], 'b');
  end
     
  def md
    puts "a, *b = 1, 2, *3"
    a, *b = 1, 2, *3  # nonStarSz 1 lsz 2 , rsz 3
    check_res( a , 1, 'a');
    check_res( b , [2,3] , 'b');
  end
     
  def me
    # L: nil, S: x, R: argscat
    puts "*a = 1, *2"
    *a = 1, *2   # nonStarSz 0 , lsz 1 rsz 2
    check_res( a , [1,2] , 'a');
  end
  def test
    ma
    mb
    mc
    md 
    me
  end
end

C629.new.test

unless $errcnt == 0; raise 'errors'; end
puts "Ok"
true
