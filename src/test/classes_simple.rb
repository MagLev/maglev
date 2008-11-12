# Two simple class constructs 

class TestA
  
  def doMath
    55 + 33    
  end
  
end

class TestB
  
  def someMethod
    "Hello"
  end

end

a = TestA.new.doMath
unless a == 88
 raise 'ERR'
end
b = TestB.new.someMethod

c = Class.new

CX = c
unless c.name == 'CX' 
  raise 'ERR'
end
true
