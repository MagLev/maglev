require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_quicksort.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

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

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_quicksort.rb'
