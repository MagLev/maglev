# file DefineMethod_a.rb,  tests for the @_st_execBridge bridge methods
#   used in Method.rb  (part of fix for Trac 811)

class CDefineMeth
  def mz
    22
  end
  def mOne(a)
    a
  end
  def mTwo(a,b)
    a + b
  end
  def mThree(a,b,c)
    a+b+c
  end
  def mzStar(*a)
    a[0]
  end
  def mOneStar(a, *b)
    a+b[0]
  end
  def mTwoStar(a,b,*c)
    a + b + c[0]
  end
  def mThreeStar(a,b,c,*d)
    a+b+c + d[0]
  end
  def mzBlk(&b)
    yield
  end
  def mOneBlk(a,&b)
    a + yield
  end
  def mTwoBlk(a,b,&c)
    a + b + yield
  end
  def mThreeBlk(a,b,c,&d)
    a+b+c + yield
  end
  def mzStarBlk(*a, &b)
    a[0] + yield
  end
  def mOneStarBlk(a, *b,&c)
    a+b[0] + yield
  end
  def mTwoStarBlk(a,b,*c,&d)
    a + b + c[0] + yield
  end
  def mThreeStarBlk(a,b,c,*d,&e)
    a+b+c + d[0] + yield
  end
  def test(exp, meth, args, &blk)
    puts "test #{exp}  #{meth.name} "
    x = meth.call(*args, &blk)
    unless x == exp ; raise 'wrong method result'; end 
  end
  def wrongNum(meth, args, &blk)
    puts " wrongNum  #{meth.name} #{args.size} "
    y = 0
    begin
      meth.call( *args, &blk)
      raise 'expected to get an ArgumentError'
    rescue ArgumentError
      y = 99
    end
    unless y == 99 ; raise 'failed to get ArgumentError';end
  end
  def am(sel)
    self.method( sel )
  end

    # all selectors [
    # :mz, :mOne, :mTwo, :mThree ,
    # :mzStar, :mOneStar, :mTwoStar , :mThreeStar ,
    # :mzBlk ,  :mOneBlk ,  :mTwoBlk ,  :mThreeBlk  ,
    # :mzStarBlk ,  :mOneStarBlk ,  :mTwoStarBlk ,  :mThreeStarBlk   ]
  
  def alltests

    test( 22, am(:mz) , [] ) 
    wrongNum(  am(:mz), [5] ) 
    test( 300, am(:mzBlk) , [] ) { 300 }

    test( 23, am(:mzStar), [ 23 ] ) 
    test( 423, am(:mzStarBlk) , [23] ) { 400 }

    test( 33 , am(:mOne), [33] )
    wrongNum(   am(:mOne), [33, 44])
    wrongNum(    am(:mOne), [])

    test( 133 , am(:mOneBlk), [33] ) { 100 }
    test( 323, am(:mOneStar), [23, 300])
    test( 323, am(:mOneStar), [23, 300, 301])

    test( 120, am(:mTwo) , [ 100, 20 ] )
    test( 120, am(:mTwo) , [ 100, 20 ] ) { 5 }
    test( 125, am(:mTwoBlk) , [ 100, 20 ] ) { 5 }
    wrongNum(   am(:mTwo) , [ 100 ])
    wrongNum(   am(:mTwo) , [ 100 , 2, 3])

    test( 125, am(:mTwoStar), [ 100, 20 , 5, 6] )
    test( 2125, am(:mTwoStarBlk), [ 100, 20 , 5, 6] ) { 2000 }

    test( 123, am(:mThree), [ 100, 20, 3 ] )
    wrongNum(  am(:mThree), [ 100, 20 ])
    wrongNum(  am(:mThree), [ 100, 20 , 3, 4])
    test(4123, am(:mThreeBlk), [ 100, 20, 3 ] ) { 4000 } 
    test(4123, am(:mThreeStar),  [ 100, 20, 3 , 4000])  { 9 }
    test(54123, am(:mThreeStarBlk),  [ 100, 20, 3 , 4000]) { 50000 }
  end
end
o = CDefineMeth.new
o.alltests

true
