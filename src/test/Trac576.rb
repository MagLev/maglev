class Spec
  def self.current
    cc = @@current
    cc
  end
  def initialize
    super
    @@current = self
  end
end

class ASpec < Spec
end

a = ASpec.new  # Should set  @@current to a
b = ASpec.current
c = Spec.current
unless b.equal?(a) ; raise 'error' ; end
unless c.equal?(a) ; raise 'error' ; end
true
