# Tests declaration of a class variable 

class TestD
  
  @@a_variable = nil
  
  def someMethod
    "Hello"
  end

end

puts TestD.new.someMethod
