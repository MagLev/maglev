
class A
  X = 5
  Y = 6
  def getx
    X
  end
  def gety
    Y
  end
end
class B < A
  X = 55
  def getx
    X
  end
  def gety
    Y
  end
  def getcs
    CS
  end
  def self.getcs
    CS
  end
end
bcls = B
class << B
  CS = 7
end

class B
  class << self
    def get_cs
      CS
    end
  end
end


begin
  B::CS 
  nil.pause
  raise 'failed'
rescue NameError
  # ok
end

ax = [ A.new.getx , A.new.gety, B.new.getx, B.new.gety , B.get_cs ]
puts "ax = #{ax.inspect}"
unless ax == [ 5, 6, 55, 6, 7 ] ; raise 'failed'; end

begin
  B.new.getcs
  nil.pause
  raise 'failed'
rescue NameError
  # ok
end
begin
  B.getcs
  nil.pause
  raise 'failed'
rescue NameError
  # ok
end

module M
end
module M
  MX = 20
  class << self
    MY = 30
  end
end
mx = M
module M
  def self.getx
    MX
  end
  def self.gety
    begin
      sx = self
      r = MY
      nil.pause
    rescue NameError
      return 'no const'
    end
    raise 'failed'
  end
  class << self
     def getyy
       MY # succeeds
     end
  end
end
unless M.getx == 20 ; raise 'failed';end
unless M.gety == 'no const' ; raise 'failed';end
unless M.getyy == 30 ; raise 'failed';end
puts 'ok'
