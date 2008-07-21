# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_sum_file.rb
# PASSES

require 'benchmark'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_sum_file '){
    100.times do
      count = 0
      File.open("random.input", "r").each_line do |line|
        count += line.to_i
      end
      puts count
    end
  }
  
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
