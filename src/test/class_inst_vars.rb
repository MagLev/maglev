# Tests declaration of a class instance variable 

class TestC
  
  @a_variable = nil
  
  def someMethod
    "Hello"
  end

end

puts TestC.new.someMethod
