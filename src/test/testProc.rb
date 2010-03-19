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

 a = [77,66]
 p[*a]    # coverage for ticket 132
 unless a.equal?(77) then raiseErr end
 unless b.equal?(66) then raiseErr end

 def proc_from
   # this form not supported
   Proc.new
 end

 def proc_fromA(&b)
   Proc.new(&b)
 end
 def proc_fromB(&b)
   begin
     p = Proc.new(b)
     rescue Exception
       # expect Gemstone error 2111
       p = 999
     else
       raise 'ERR'
   end
   p
 end

 p = Proc.new { "hello" }
 r = p.call
 unless r == "hello" then raiseErr end

 # p = proc_from  # not supported yet

 p = proc_fromA { "goodbye" }
 r = p.call
 unless r == "goodbye" then raiseErr end

 p = proc_fromB { "notime" }
 unless p == 999 then raiseErr end

 a = 0
 p = Proc.new { | x | a = a + x }
 p.call(30)
 unless a == 30 then raiseErr end

 p = Proc.new { | x | a = x }
 p.call
 unless a.equal?(nil) then raiseErr end


# Ensure passed blocks have arity defined
def test_arity(expected, msg, &block)
  actual = block.arity
  raise "test_arity: #{msg}: expected #{expected} actual: #{actual}" unless actual == expected
end

test_arity(-1, "A") { true }
test_arity(1, "A") { |x| true }
test_arity(2, "A") { |x,y| true }

# coverage added for 1.8.7 syntax changes (last arg to a block can have &)
#
sel = :call  
pa = Proc.new {|&blk| blk.send( sel ) }
x = pa.send( sel ) {150 + 100 }
unless x == 250 ; raise 'error'; end

pb = Proc.new {|aa, &blk| blk.call(aa, 5) }
y = pb.call(300 ) {|m,n| m + n }
unless y == 305 ; raise 'error'; end
true
