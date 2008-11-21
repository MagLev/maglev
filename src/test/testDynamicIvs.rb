class Object
  def initObj(v)
    @a_obj = v
  end
end
module MB
  @a_mb = 102
  def ma
    @aa = 103   # dynamic iv store
  end
  def self.mb
    @bb = 104   # class instvar store
  end
  def mc
    @cc = 105  # dynamic iv
  end
  def self.check(v)
    unless @bb == 104 ; raise 'ERR' ; end
    unless @a_obj == v ; raise 'ERR' ; end 
  end
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
o = CA.new
o.ma
MB.mb
MB.initObj(21)
o.mc
o.md(22)
CA.me
o.check(22)
CA.check
MB.check(21)
true
