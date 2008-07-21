# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_quicksort.rb
# FAILS

require 'benchmark'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_quicksort '){
    class Array
      def qsort
        return [] if self.empty?
        pivot, *tail = self
        (tail.select {|el| el < pivot }).qsort + [pivot] +
          (tail.select {|el| el >= pivot }).qsort
      end  
    end
  
    array = File.read("random.input").split(/\n/).map!{|n| n.to_i }
    puts "Quicksort verified." if array.qsort == array.sort
  
  }

end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
