# 
# Benchmark File.each_line
#

require 'benchmark'

5.times do
  f = File.open("random.input", "r")
  puts Benchmark.measure('bm_each_line'){
    f.each_line do |line|
      # noop
      end
  }
end


# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
