# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_so_sieve.rb
#

require 'benchmark'

Benchmark.bm(12) do |bmr|
  
  bmr.report('bm_so_sieve '){
    num = 4000
    count = i = j = 0
    flags0 = Array.new(8192,1)
    k = 0
    while k < num
      k+=1
      count = 0
      flags = flags0.dup
      i = 2
      while i<8192
        i+=1
        if flags[i]
          # remove all multiples of prime: i
          j = i*i
          while j < 8192
            j += i
            flags[j] = nil
          end
          count += 1
        end
      end
    end
    count
  }
 
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
