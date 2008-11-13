require File.expand_path('simple', File.dirname(__FILE__))

# TODO: 2008-11-04: this currently fails because Class.new is not allowed.
Struct.new("Customer", :name, :address)
dave = Struct::Customer.new("Dave", "123 Main")
test(dave.name,    "Dave",     "Struct A")
test(dave.address, "123 Main", "Struct B")

report
true
