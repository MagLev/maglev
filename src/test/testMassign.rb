def expect(act, exp)
  unless act == exp 
    puts 'error'
    nil.pause
  end
end

 a , b  = 1, 2, 3
   expect(a, 1)
   expect(b, 2)
 y = (a , b  = 1, 2, 3)
   expect(y, [1,2,3])
   expect(a, 1)
   expect(b, 2)

 a,b,c = 1,2
   expect(a, 1)
   expect(b, 2)
   expect(c, nil)
 y = (a,b,c = 1,2)
   expect(y, [1,2]) 
   expect(a, 1)
   expect(b, 2)
   expect(c, nil)

 c, *d  = 4, 5, 6
   expect(c ,4)
   expect(d , [ 5, 6 ] )
 y = ( c, *d  = 4, 5, 6 )
   expect(y , [4,5,6])
   expect(c ,4)
   expect(d , [ 5, 6 ] )


 x = [ 9, 8, 7]
 a , b, * = x[0]
   expect( a , 9 )
   expect( b , nil )

 b = 22
 c = 33
 b,c = c,b
   expect( c , 22 )
   expect( b , 33 )

 x = [1,2,3,4]
 y = x[0,2] += [5]
   expect( y , [1,2,5] )
   expect( x , [1,2,5,3,4] )

 x = [22,[33,44]]
 b,(c,d) = x
   expect( b , 22 )
   expect( c , 33 )
   expect( d , 44 )
 
 x = [11,[22,[33,44]]]
 a,(b,(c,d)) = x
   expect( a , 11 )
   expect( b , 22 )
   expect( c , 33 )
   expect( d , 44 )

 *b = [1] 
 expect( b , [[1]] )

 *w = 5
 expect( w , [5] )

 * = (k=5),(j=6)   # useless but allowed , and don't skip rhs side effects
 expect( k , 5)
 expect( j , 6)

 *w = [*[7]]
 expect( w , [[7]] )

 *j = [1,2,3]
 expect( j , [[1,2,3]] )

 *x = (1..7).to_a
 expect(x , [[1, 2, 3, 4, 5, 6, 7]] )

 a,b,*c = [*[8,9]] 
 expect(a, 8)
 expect(b, 9)
 expect(c , [])

 a,b,*c = []
 expect(a, nil)
 expect(b, nil)
 expect(c, [])

 *a = *[1]
 expect(a , [1])

 a,b,*c = [1,2]
 expect( [a,b,c] , [1,2,[]])

 a,b,*c = *1
 expect( [a,b,c] , [1, nil, []] )

 *a = 1, 2, 3
 expect( a , [1, 2, 3] )

 a,(b,(c,d)) = 1,[2,[3,4]]
 expect( [a, b, c, d ], [1,2,3,4] )

 a , = *[1]
 expect (a , 1)

 a,(b,(c,d)) = [1,[2,[3,4]]]
 expect( [a, b, c, d],  [1,2,3,4] )


class Holder
  def get
    @val
  end
  def set(v)
    @val = v
  end
  def initialize
    @val = 987
  end
  def set2(a, b)
    foo, @ivb = a, b 
  end
  def getb
    @ivb
  end
  attr_accessor :val
end

def raiseErr
  raise 'ERR'
end


 # cases from debugging ticket 112
 h = Holder.new		# RubyAttrAssignNode for store to h.val 
 h.val , b = 33 , 44
   unless b == 44 then raiseErr end
   unless h.get == 33 then raiseErr end

 p = 345
 a,(*b),c = 5,p

 h.set2(20,30)  # cover a pattern seen in Sinatra , instVar on LHS of masgn
 x = h.getb
   unless x == 30 then raiseErr end

 true
