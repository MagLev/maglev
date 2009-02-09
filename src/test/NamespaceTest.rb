# This test case came from RbYaml.  MagLev used to complain
# that
#   (A) There was no global namespace for the anonymous class created
#       for the structs
#   (B) The second one gave an error that #'' was already defined in the
#       parent namespace.
A = Struct.new(:token_number, :required, :column)
B = Struct.new(:baz)
a = A.new(12, true, 4)
raise "Failed token_number" unless a.token_number == 12
raise "Failed required"     unless a.required
raise "Failed column"       unless a.column == 4

b = B.new("b")
raise "Failed B" unless b.baz == "b"

true
