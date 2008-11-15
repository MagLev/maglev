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
true

