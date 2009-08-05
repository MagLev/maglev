$dir = File.dirname(__FILE__)
require File.expand_path('simple', $dir)

# ###########################
# First, test Module#autoload
# ###########################

# Make sure nothing is registered yet
module AutoA
end
test(AutoA.autoload?(:AutoB), nil, "[1] autoload? before autoload")
test(defined? :AutoA,   'expression',  "[2]")
test(defined? AutoA::AutoB, nil, "[3]")
test(defined? AutoA::AutoB, nil, "[4]")

module AutoA
  autoload(:AutoB, $dir + '/AutoloadHelper.rb')
end


test(defined? :AutoA,   'expression',  "[5] :A should be defined before")
test(defined? AutoA::AutoB, "constant", "[6] A::B should not be defined before autoload")
test(defined? AutoA::AutoB, "constant", "[7] calling defined? should NOT have defined A::B")
test($marker, nil, "[8] $marker should be nil before the autoload")

# Now reference A::B to force the autoload
AutoA::AutoB.new

test(defined? :AutoA, 'expression' ,   "[9] :A should be defined after")
test(defined? AutoA::AutoB, 'constant', "[10] A::B should be defined after autoload")
test($marker, true, "[11] $marker should not be nil after the autoload")

# ###########################
# Now test the Kernel.autoload
# ###########################

test(autoload?(:AutoFoo), nil, '[12] Kernel autoload?')

autoload(:AutoFoo, $dir + '/AutoloadHelper2.rb')
ma = AutoFoo::AConstant
test(ma , "Some value", '[13] Kernel autoload')

report
true
