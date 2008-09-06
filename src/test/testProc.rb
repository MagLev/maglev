 def raiseErr
  raise 'ERR'
 end

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

