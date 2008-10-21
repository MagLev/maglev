# Test that require honors a new, relative path shifted onto the front of
# $: This assumes we are running the test from the top level git dir.
$:.unshift 'src/test/lib'
puts "$: #{$:.inspect}"
require 'foobar'
puts "After require"

