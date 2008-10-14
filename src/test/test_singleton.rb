 s = 'abc'
 unless s.class.equal?(String)
   raise 'ERR'
 end
 unless s.class.class.equal?(Class)
   raise 'ERR'
 end
 $y = 99
 def s.foo   # creates singleton via Sexp  :defs , RubyDefsNode
   $y = 88
 end
 s.foo
 unless $y == 88
   raise 'ERR'
 end

 t = 'def'
 begin
   t.foo
   rescue NoMethodError
     $y = 77      
   else
     $y = 76
 end
 unless $y == 77
   raise 'ERR'
 end

 r = 'nop'
 ca = r.class
 cb = class << r    # Sexp :sclass , RubySClassNode
   def foob
     $y = 101
   end
 end
 r.foob
 unless $y == 101
   raise 'ERR'
 end
 $y = 0
 'newstr'.foob
 unless $y == 101
   raise 'ERR'
 end

 module M
   def mma
     $y = 203
   end
   def mmaa
     $y = 205
   end
 end
 class F
   include M
 end
 class << F
   include M
 end
 $y = 2
 F.new.mma 
 unless $y == 203
   raise 'ERR'
 end
 F.new.class.mmaa
 unless $y == 205
   raise 'ERR'
 end
 true

