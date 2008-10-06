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
  attr_accessor :val
end

def raiseErr
  raise 'ERR'
end

 a , b  = 1, 2, 3
 unless a == 1 then raiseErr end
 unless b == 2 then raiseErr end
 c, *d  = 4, 5, 6
 unless c == 4 then raiseErr end
 unless d == [ 5, 6 ] then raiseErr end

 # TODO , more test cases from p97..100 of OReilly Flanagan & Mats book"

 # cases from debugging ticket 112
 h = Holder.new		# RubyAttrAssignNode for store to h.val 
 h.val , b = 33 , 44
 unless b == 44 then raiseErr end
 unless h.get == 33 then raiseErr end

 p = 345
 a,(*b),c = 5,p

 x = [11,[22,[33,44]]]
 a,(b,(c,d)) = x
 unless a == 11 then raiseErr end 
 unless b == 22 then raiseErr end 
 unless c == 33 then raiseErr end 
 unless d == 44 then raiseErr end 

 x = [1,2,3,4]
 y = x[0,2] += [5]
 unless y == [1,2,5] then raiseErr end
 unless x == [1,2,5,3,4] then raiseErr end
 
 x = [ 9, 8, 7]
 a , b, * = x[0]
 unless a == 9 then raiseErr end
 unless b == nil then raiseErr end

 true
