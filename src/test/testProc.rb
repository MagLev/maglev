 def raiseErr
  raise 'ERR'
 end

 a = 90
 p = Proc.new { | y, *x | a = x[0]  }
 p.call
 unless a.equal?(nil) then raiseErr end

 a = 89
 p = Proc.new { | *x | a = x[0]  }
 p.call
 unless a.equal?(nil) then raiseErr end

 a = 88
 p = Proc.new { | x | a = x  }
 p.call
 unless a.equal?(nil) then raiseErr end

 a = 87
 b = 87
 p = Proc.new { | x , y | a = x ; b = y }
 p.call
 unless a.equal?(nil) then raiseErr end
 unless b.equal?(nil) then raiseErr end

 a = [99,98]
 p[*a]
 unless a.equal?(99) then raiseErr end
 unless b.equal?(98) then raiseErr end

 def proc_from
   # this form not supported
   Proc.new 
 end

 def proc_fromA(&b)
   Proc.new(&b) 
 end
 def proc_fromB(&b)
   Proc.new(b) 
 end

 p = Proc.new { "hello" }
 r = p.call
 unless r = "hello" then raiseErr end
 
 # p = proc_from  # not supported yet

 p = proc_fromA { "goodbye" }
 r = p.call
 unless r = "goodbye" then raiseErr end

 p = proc_fromB { "notime" }
 r = p.call
 unless r = "notime" then raiseErr end

 a = 0
 p = Proc.new { | x | a = a + x }
 p.call(30)
 unless a == 30 then raiseErr end

 p = Proc.new { | x | a = x }
 p.call
 unless a.equal?(nil) then raiseErr end



