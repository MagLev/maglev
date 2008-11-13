module A
  class B
    Number = 47
  end
end

class D < A::B
  def number
    Number  
  end
end

n = D.new.number
unless n == 47
  raise 'ERR'
end

e = 0
begin
  x =  :File::TEST 
rescue TypeError
  e = 22
end
unless e == 22
  raise 'ERR'
end

V = 3
begin
  V::W = 5
rescue TypeError
  e = 33
end
unless e == 33
  raise 'ERR'
end

begin
  class nil::Foo
  end
rescue TypeError
  e = 44
end
unless e == 44
  raise 'ERR'
end
 

true
