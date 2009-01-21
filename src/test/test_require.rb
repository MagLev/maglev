# Test that require honors a new, relative path shifted onto the front of
# $: This assumes we are running the test from the top level git dir.
#

# This test indicates success by not raising an exception
pa = $:
pa = pa.dup
$:.unshift 'src/test/lib'
puts "$: #{$:.inspect}"
pb = $:
require 'foobar'
# ================================================


require File.expand_path('simple', File.dirname(__FILE__))

# Check that $" does not have any of the bootstrap files
#test($".grep(/bootstrap/), [], "Found bootstrap files on $\": #{$"}")
#test($LOADED_FEATURES.grep(/bootstrap/), [], "Found bootstrap files on $LOADED_FEATURES: #{$LOADED_FEATURES}")
#test($".eql? $LOADED_FEATURES, true, "$LOADED_FEATURES not equal $\""")


# ensure that requiring a non-existent file raises an error
begin
  require 'this_does_not_exist'
  # Failed to raise an exception!
  failed_test("require 'this_does_not_exist' should raise load error: NO EXCEPTION", LoadError, nil)
rescue LoadError => le
  # OK!  We want a LoadError
rescue Exception => e
  # Wrong error...
  failed_test("require 'this_does_not_exist' should raise load error: not #{e}", LoadError, e)
end

# ensure that loading a non-existent file raises an error
begin
  load 'this_does_not_exist'
  # Failed to raise an exception!
  failed_test("load 'this_does_not_exist' should raise load error: NO EXCEPTION", LoadError, nil)
rescue LoadError => le
  # OK!  We want a LoadError
rescue Exception => e
  # Wrong error...
  failed_test("load 'this_does_not_exist' should raise load error: not #{e}", LoadError, e)
end


# Ensure that when we start the test, that our fixture is NOT already loaded
$foo = 0
test($".include?('foo'),          false, 'A: foo already on $"')
test($:.include?('src/test/lib'), true,  "B: src/test/lib not on $:")

# Test require only loads foo once
require 'foo'
test($foo,                     1, "C: foo not loaded first time")
test($".include?('foo.rb'), true, "D: foo.rb not put on $\" #{$".inspect}")

require 'foo'
test($foo,                     1, "E: require foo: #{$foo}")

require 'foo.rb'
test($foo,                     1, "F: require foo.rb: #{$foo}")

# Test that load will re-load the file
load 'foo.rb'
test($foo,                     2, "G: load foo.rb: #{$foo}")

# Test that load needs the .rb extension
begin
  load 'foo'
  failed_test("load foo should not find foo.rb", 0, 1)
rescue Exception => e
  # OK
end
test($foo,                     2, "H: load foo: #{$foo}")


# puts "load_path: #{load_path.inspect}"
# puts "foo:       #{$foo}"


Gemstone.abortTransaction  # Clear anything we might have done...
report
