# Test that doing a puts of a recursive array, prints out correctly.
#
# This version of the test uses StringIO, Trac592.rb uses STDOUT.
# This file tests that puts (on StringIO), works correctly.

require 'stringio'
require File.expand_path('simple', File.dirname(__FILE__))

# # Unset MAGLEV_OPTS so that -d doesn't mess up the output
# result = `MAGLEV_OPTS= maglev-ruby -e 'a = [:a, :b, :c]; a<<a; puts a'`

a = [:a, :b, :c]
a << a

sio = StringIO.new
sio.puts(a)
result = sio.string
# p result
test(result, "a\nb\nc\n[...]\n", "recursive case")

report
