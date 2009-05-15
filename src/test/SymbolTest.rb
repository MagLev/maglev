require File.expand_path('simple', File.dirname(__FILE__))

# Ensure that all_symbols is now a class method (was an instance method)
# and that the _nobridge versions of the methods work.
test(:s.to_i.class, Fixnum, 'to_i')
test(Symbol.all_symbols.length > 0, true, 'all_symbols')

# Because in ST Symbol derives from String, but in Ruby it does not,
# need to ensure respond_to? works correctly
test(:xyz.respond_to?(:to_str), false, "symbol should not respond to :to_str")
test(:xyz.respond_to?(:to_s), true, "symbol should respond to :to_s")
test(:x == "x", false, ":x == 'x'")
test("x" == :x, false, "'x' == :x")

report
true
