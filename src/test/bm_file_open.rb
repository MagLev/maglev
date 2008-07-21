# 
# Benchmark File.open
#

require 'benchmark'

puts Benchmark.measure('bm_file_open '){
  100.times do
    f = File.open("random.input", "r")
  end
}

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
