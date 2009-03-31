class C
  @class_iv = 42
  attr_accessor :iv1, :iv2
end

o1 = C.new
o2 = C.new
raise "Fail A1" unless o1.instance_variables == []
raise "Fail A2" unless o2.instance_variables == []

o1.iv1 = :foo
raise "Fail B1" unless o1.instance_variables == ["@iv1"]
raise "Fail B2" unless o2.instance_variables == []

o2.iv2 = :bar
raise "Fail B1" unless o1.instance_variables == ["@iv1"]
raise "Fail B2" unless o2.instance_variables == ["@iv2"]


