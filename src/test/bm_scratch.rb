# 
# Benchmark template 
#  

require 'benchmark'

Benchmark.bm(12) do |bmr|

 
  bmr.report("example"){
    puts "Block to be benchmarked"  
  }
 
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
