# puts "#{__FILE__}: Is transient?: #{Maglev.transient?}"
class C003
  XYZ = 45
  @@class_variable = 1
  @class_instance_variable = 2

  def self.init(n=22)
    @@class_variable = n
  end

  def self.class_variable
    @@class_variable
  end
  def self.class_instance_variable
    @class_instance_variable
  end
  def initialize
    @instance_variable = 3
  end
  def instance_variable
    @instance_variable
  end
end

p C003.init
p C003.class_variable
p C003.class_instance_variable
p C003.new.instance_variable
