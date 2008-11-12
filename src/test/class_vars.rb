# Tests declaration of a class variable 

class TestD
  
  @@a_variable = 88
  
  def setCv
    @@a_variable = 99
  end

  def fetchCv
    @@a_variable
  end

end

o = TestD.new 
r = o.fetchCv
unless r == 88
  raise 'ERR'
end
o.setCv
r = o.fetchCv
unless r == 99
  raise 'ERR'
end
true
