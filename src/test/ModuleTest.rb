# Tests for methods in Kernel.rb
require File.expand_path('simple', File.dirname(__FILE__))


# Test that assignment to a constant sets the class name correctly
Foo = Class.new
test(Foo.class, Class, 'Foo.class == Class')

test(Math.constants.include?("PI"), true, 'Math::PI')
test(Math.constants.include?("E"), true,  'Math::E')
test(Math.constants.length,           2,  'Math.constants.length')

module M
end

test(M.constants.length, 0, 'M.constants 1')
M.const_set(:FOO, 12)
test(M.constants.length, 1, 'const_set')
test(M.const_get(:FOO), 12, 'const_get')
test(M.constants, ['FOO'],  'constants')

Gemstone.abortTransaction if defined? RUBY_ENGINE

report

true
