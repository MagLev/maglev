V = 3
a = V
unless a == 3 ; raise 'err' ; end

module M
  X = 9
end
b = M::X
unless b == 9 ; raise 'err' ; end
M::Y = 10
c = M::Y
unless c == 10 ; raise 'err' ; end

begin
  V::W = 5
  nil.pause
rescue TypeError
  e = 33
end
unless e == 33; raise 'ERR'; end
begin
  b = V::W
  nil.pause
rescue TypeError
  e = 44
end
unless e == 44; raise 'ERR'; end

o = Object.new
class << o
   AB = 99
end
 
class << o
  unless AB == 99 ; raise 'err' ; end
end
 
class << o
  unless self::AB == 99 ; raise 'err' ; end
end
puts "ok"
true
