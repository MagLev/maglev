$dir = File.dirname(__FILE__)
require File.expand_path('simple', $dir)

# ###########################
# First, test Module#autoload
# ###########################

module AutoA
  autoload(:AutoB, $dir + '/AutoloadHelper.rb')
end

test(defined? :AutoA,   'expression',  ":A should be defined before")
test(defined? AutoA::AutoB, nil, "A::B should not be defined before autoload")
test(defined? AutoA::AutoB, nil, "calling defined? should NOT have defined A::B")
test($marker, nil, "$marker should be nil before the autoload")

# Now reference A::B to force the autoload
AutoA::AutoB.new

test(defined? :AutoA, 'expression' ,   ":A should be defined after")
test(defined? AutoA::AutoB, 'constant', "A::B should be defined after autoload")
test($marker, true, "$marker should not be nil after the autoload")

# ###########################
# Now test the Kernel.autoload
# ###########################

test(autoload?(:AutoFoo), nil, 'Kernel autoload?')

autoload(:AutoFoo, $dir + '/AutoloadHelper2.rb')
ma = AutoFoo::AConstant 
test(ma , "Some value", 'Kernel autoload')

true
