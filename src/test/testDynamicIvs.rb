class Object
  def initObj(v)   # cls == Object
    @a_obj = v
  end
end
module MB
  @a_mb = 102	# cls == Module
  def ma
    @aa = 103   # dynamic iv store # cls == 
  end
  def self.mb
    @bb = 104   # class instvar store # res == 2
  end
  def self.check(v)
    unless @bb == 104 ; raise 'ERR' ; end
    unless @a_obj == v ; raise 'ERR' ; end 
  end
end
module MC
  def block
    @block
  end
  module_function :block
  mc = self

  def block=(block)
    @block = block
  end
  module_function :block=

end
class CA
  include MB
  def md(v)
    @dd = 106  # instvar
    # self.initObj(v)
  end
  def self.me
    @ee = 107  # class instvar
  end
  def check(v)
    unless @dd == 106 ; raise 'ERR' ; end
    # unless @a_obj == v ; raise 'ERR' ; end
  end 
  def self.check
    unless @ee == 107 ; raise 'ERR' ; end
  end
end
class CC
 include MC
 def getBlock
   @block
 end
end
o = CA.new
o.ma
MB.mb
MB.initObj(21)

o.md(22)
CA.me
o.check(22)
CA.check
MB.check(21)

oc = CC.new
oc.block=(95)
MC.block=(96)
unless oc.getBlock == 95 ; raise 'ERR'; end
unless oc.block == 95 ; raise 'ERR'; end
unless MC.block == 96 ; raise 'ERR'; end

true

