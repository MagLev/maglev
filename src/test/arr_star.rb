# 
# Test array *
#

class Array
  def test
    puts 'self is ' + self.to_s  
    a1 , * a2 = self
    puts 'a1 is ' + a1.to_s + ', a2 is ' + a2.to_s 
  end  
end

ary = ["a", "b", "c", "d"]
ary.test



