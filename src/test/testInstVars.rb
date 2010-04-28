class C
  def get
    self.class
    @iva
  end
  def send_get
    self.class
    @iva
  end
  def set(v)
    @iva = v
  end
  def send_set(v)
    self.class
    @iva = v
  end
 
  def get_blk
    res = []
    [2].each do |x|
      res << @iva
    end
    res
  end 

  def set_blk
    [22].each do |x|
      @iva = x
    end
  end 
end

puts "starting main"
o = C.new
o.set(9)
y = o.get
unless y == 9 ; raise 'err' ; end
o.send_set(10)
y = o.send_get
unless y == 10 ; raise 'err' ; end
o.set_blk
y = o.get
unless y == 22 ; raise 'err' ; end
y = o.get_blk
unless y == [22]; raise 'err' ; end
true

# coverage for bad bytecode optimimzation seen in rack gem under rails
class C
  # second opening, dynamic IVs
  def ma
    @dvc = mb(@dva, @dvb)
    x = @dvc
    x 
  end  
  def mb(a,b)
    a + b
  end
  def initialize(a,b)
    @dva = a
    @dvb = b
  end
end

c = C.new(9,200)
x = c.ma
unless x == 209 ; raise 'fail'; end
true

