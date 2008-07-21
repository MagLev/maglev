# 
# Benchmarks File.read 
#

require 'benchmark'

puts Benchmark.measure('bm_file_read '){
  100.times do
    File.read("random.input")
  end
}

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
