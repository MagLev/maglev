class Spec
  def self.current
    @@current
  end
  def initialize
    super
    @@current = self
    puts "@@current is now: #{@@current}"
  end
end

class ASpec < Spec
end

a = ASpec.new  # Should set  @@current to a
p Spec.current
