require File.expand_path('simple', File.dirname(__FILE__))

Struct.new("Customer", :name, :address)
dave = Struct::Customer.new("Dave", "123 Main")

test(dave.name,    "Dave",     "Struct A")
test(dave.address, "123 Main", "Struct B")
test(Struct.constants.include?("Customer"), true, "Struct C")

report
true
