# 
# Tests Benchmark.realtime
#  

require 'benchmark'

	puts Benchmark.realtime { "a"*1_000_000 }
	
# Prevent saving this into RubyContext
Gemstone.abortTransaction rescue nil
