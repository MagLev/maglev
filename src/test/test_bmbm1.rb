# 
# Tests Benchmark bmbm and report methods 
#

require 'benchmark'

Benchmark.bmbm do |bmr|
 
  bmr.report {
    i = 0
    def m a, b
    end

    while i<6000000 # benchmark loop 2
      i+=1
      m 100, 200
    end
  }
 
end

# Prevent saving this into RubyContext
Gemstone.abortTransaction rescue nil
