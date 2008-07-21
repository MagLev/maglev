# 
# Benchmark block of code from ruby-benchmark-suite/core-library/bm_vm2_regexp.rb
#

require 'benchmark'

Benchmark.bm(12) do |bmr|

  bmr.report('bm_vm2_regexp '){
    i=0
    str = 'xxxhogexxx'
    while i<6000000 # benchmark loop 2
      /hoge/ =~ str
      i+=1
    end 
  }
 
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil

