# test for Behavior#instance_method, Object#method, Module#define_method
class CDefineMeth
  def initialize
    @num = 20
  end
  def set(n)
    @num = n
  end
  def fetch(bias)
    @num + bias
  end
  def self.getm
    instance_method(:fetch)
  end
  def getm
    method(:fetch)
  end
  define_method( :fetchx , instance_method(:fetch))

  define_method( :fetchblk ) { |a| a + 1000 }
end

o = CDefineMeth.new
um = CDefineMeth.getm
m = o.getm
a = o.fetch(100)
unless a == 120 ; raise 'Error'; end 
c = m.call(200)  # expect 220
unless c = 220 ; raise 'Error'; end 
b = o.fetchx(300)
unless b = 320 ; raise 'Error'; end 

o.set(30)
d = m.call(200)  # expect 230
unless d == 230 ; raise 'Error'; end 
e = o.fetchblk(40)
unless e == 1040 ; raise 'Error'; end 

true
