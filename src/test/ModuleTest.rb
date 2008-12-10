# Tests for methods in Kernel.rb
require File.expand_path('simple', File.dirname(__FILE__))

# Test that assignment to a constant sets the class name correctly
Foo = Class.new
test(Foo.class, Class, 'Foo.class == Class')

test(Math.constants.include?("PI"), true, 'Math::PI')
test(Math.constants.include?("E"), true,  'Math::E')
test(Math.constants.length,           2,  'Math.constants.length')

# Test const_defined?, const_get and const_set
module M
end

test(M.constants.length,         0, 'M.constants 1')
test(M.const_defined?(:FOO), false, 'const_defined? A')

M.const_set(:FOO, 12)

test(M.constants.length,        1, 'const_set A')
test(M.constants.length,        1, 'const_set B')
test(M.const_get(:FOO),        12, 'const_get C')
test(M.const_defined?(:FOO), true, 'const_defined? B')
test(M.constants,         ['FOO'], 'constants 2')


# Test removing constants
#
# Right now, we don't have module_eval working, so instead of this:
#    M.module_eval { remove_const(:FOO) }
# we'll do it all in a class

class C
  def self.remove_my_const(sym)
    remove_const(sym)
  end
end

test(C.constants.length,         0, 'remove_const A')
test(C.const_defined?(:FOO), false, 'remove_const B')

C.const_set(:FOO, 55)

test(C.constants.length,         1, 'remove_const C')
test(C.const_defined?(:FOO),  true, 'remove_const D')

C.remove_my_const(:FOO)

test(C.constants.length,         0, 'remove_const E')
test(C.const_defined?(:FOO), false, 'remove_const F')

# Ensure const_missing is called.
class C
  def self.const_missing(sym)
    sym.to_s
  end
end

test(C.const_get(:NOT_DEFINED), "NOT_DEFINED", 'const_missing A')
test(C.const_get(:STILL_NOT_DEFINED), "STILL_NOT_DEFINED", 'const_missing B')
test(C.const_get(:STILL_NOT_DEFINED), "STILL_NOT_DEFINED", 'const_missing C')

# Ensure default const_missing raises a NameError
begin
  Object.const_get(:XYZ_ABC)
  failed_test('Expected Name Error', NameError, nil)
rescue NameError
  # Ok!
rescue Exception => e
  # Failed...wrong exception
  failed_test('Expected Name Error', NameError, e)
end


##########################
#  Tests for  module_eval
##########################
class Thing
end

# Test for the string version
a = %q{def testEval() 123 end}
Thing.module_eval(a)
test(Thing.new.testEval, 123, 'module_eval of string')


# Test the block version
Thing.module_eval do
  def testBlockEval
    456
  end
end
test(Thing.new.testBlockEval, 456, 'module_eval of block')

Thing.class_eval do
  def testClassEval
    789
  end
end
test(Thing.new.testClassEval, 789, 'class_eval of block')

test(Thing.module_eval { 1 + 1 }, 2, 'module_eval of simple block')

def test_eval_with_tilde
  a = $~
  r = Thing.module_eval( ' /de/ =~ "abcded" ' )
  unless r == 3 ; raise 'Err' end;
  b = $~
  unless a == nil ; raise 'Err' end;
  unless b.class.equal?(MatchData); raise 'Err' end;
  true
end
test( test_eval_with_tilde() , true, 'eval with tilde' )

report
Gemstone.abortTransaction if defined? RUBY_ENGINE
true

