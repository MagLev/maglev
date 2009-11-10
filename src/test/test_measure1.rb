#
# Tests Benchmark.measure
#

require 'benchmark'

  puts Benchmark.measure { "a"*1_000_000 }

# Prevent saving this into RubyContext
Maglev.abort_transaction rescue nil
