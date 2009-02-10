# ###############################################################
# This test case came from RbYaml.  MagLev used to complain
# ###############################################################
# that
#   (A) There was no global namespace for the anonymous class created
#       for the structs
#   (B) The second one gave an error that #'' was already defined in the
#       parent namespace.
X = Struct.new(:token_number, :required, :column)
Y = Struct.new(:baz)
a = X.new(12, true, 4)
raise "Failed token_number" unless a.token_number == 12
raise "Failed required"     unless a.required
raise "Failed column"       unless a.column == 4

b = Y.new("b")
raise "Failed B" unless b.baz == "b"

# ###############################################################
# This test from Section 7.9: Constant Lookup of TRPL
# ###############################################################
module Kernel
  A = B = C = D = E = F = "defined in kernel"
end
A = B = C = D = E = "defined at toplevel"

class Super
  A = B = C = D = "defined in superclass"
end

module Included
  A = B = C = "defined in included module"
end

module Enclosing
  A = B = "defined in enclosing module"

  class Local < Super
    include Included
    A = "defined Locally"

    raise "Bad A #{A}" unless A == "defined Locally"
    raise "Bad B #{B}" unless B == "defined in enclosing module"
    raise "Bad C #{C}" unless C == "defined in included module"
    raise "Bad D #{D}" unless D == "defined in superclass"
    raise "Bad E #{E}" unless E == "defined at toplevel"
    raise "Bad F #{F}" unless F == "defined in kernel"

    # The list of modules searched, in the order searched
    #
    # TODO: MagLev doesn't yet support this...
#     expected = [Enclosing::Local, Enclosing, Included, Super, Object, Kernel]
#     search   = (Module.nesting + self.ancestors + Object.ancestors).uniq
#     raise "expected #{expected.inspect} actual: #{search.inspect}" unless expected == search
  end
end

true
