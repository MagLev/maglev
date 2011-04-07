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

m = M
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
  TOLERANCE = 33 unless self.const_defined?(:TOLERANCE)
  TOLERANCE = 44 unless self.const_defined?(:TOLERANCE)
end

test(C.const_get(:NOT_DEFINED), "NOT_DEFINED", 'const_missing A')
test(C.const_get(:STILL_NOT_DEFINED), "STILL_NOT_DEFINED", 'const_missing B')
test(C.const_get(:STILL_NOT_DEFINED), "STILL_NOT_DEFINED", 'const_missing C')
test(C.const_get(:TOLERANCE), 33,      'const_defined?  C')

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


# Do some tests of callbacks
$extended = Hash.new
module M27
  def self.extended(obj)
    #puts "#{self} extended #{obj}"
    $extended[self] = obj
    super
  end
  def self.extend_object(obj)
    puts "#{self} extend_object #{obj}\n"
  end
end

x = Object.new
x.extend(M27)
test($extended[M27], x, "x.extend(M)")

class C27
  extend M27
end
test($extended[M27], C27, "C27 extend M27")

# Ensure that a module_function w/o parameter gets shut off at the end
# of a particular module opening.  This test passes if no exception raised.
module JSON
  module_function
  def foo
    puts "foo"
  end
end

# This module opening should NOT have the module_function in effect
module JSON
  def bar
    puts "bar"
  end
end


JSON.foo
begin
  JSON.bar
  raise "Expecting NoMethodError for JSON.bar"
rescue NoMethodError
  # ok
end


# Test that append_feature gets called before inherited and that
# the proper include stuff is done/not-done

$append_features_called = nil
$included_called = nil
module MAppendFeatures
  def self.append_features(other)
    puts "Enter append"
    raise "included already called" unless $included_called.nil?
    raise "append_features already called" unless $append_features_called.nil?
    $append_features_called = true
    super(other)
    puts "Done append"
  end

  def self.included(other)
    puts "Enter included"
    raise "append_features not called" unless $append_features_called.equal?(true)
    raise "included already called" unless $included_called.nil?
    $included_called = true
    puts "Done included"
  end
end

class CAppendFeatures
  include MAppendFeatures
end
raise "append_features not called" unless $append_features_called
raise "included not called" unless $included_called


# The block passed to new was not being passed to the initialize method
class MRSpec < Module
  def initialize(*args, &block)
    raise "No block passed to #{self}.initialize" unless block_given?
  end
end

x = MRSpec.new(123) { "a block" }

#####################################################
# Test that undef_method accepts multiple parameters
class CXYZZY
  def x
  end

  def y
  end

  def z
  end
end

raise "Fail A" unless CXYZZY.instance_methods(false).size == 3

class CXYZZY
  # Maglev throws an argument error here
  undef_method :x, :y
end

raise "Fail B" unless CXYZZY.instance_methods(false).size == 1

# Test method source retrieval
# Check the method source is found in a number of instances
class C
  def x;      "x";      end
  def self.y; "self.y"; end
end

test(C.__gs_method('x', true).nil?,  false, "C.x true")
begin
  test(C.__gs_method('x', false), nil, "C.x false")
rescue
  # ok
end

test(C.__gs_method('y', false).nil?,  false, "C.y false")
begin
  test(C.__gs_method('y', true), nil, "C.y true")
rescue
  # ok
end

# Test with and without a singleton class
test(Array.__gs_method(:new, false).nil?, false, 'Array.new no singleton')
class Array
  class << self
    def foo
    end
  end
end
test(Array.__gs_method(:new, false).nil?, false, 'Array.new after singleton')
test(Array.__gs_method(:foo, false).nil?, false, 'Array.foo no singleton')

################### Report and clean up #####################
report
Maglev.abort_transaction if defined? RUBY_ENGINE
true

