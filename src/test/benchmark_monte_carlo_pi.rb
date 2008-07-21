# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_monte_carlo_pi.rb
# PASSES

require 'benchmark'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_monte_carlo_pi '){
    sample = 10_000_000
    count = 0
  
    sample.times do
      x = rand
      y = rand
      if x * x + y * y < 1
        count += 1
      end
    end
  
    pi = 4.0 * count / sample
    puts pi
  }
  
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
