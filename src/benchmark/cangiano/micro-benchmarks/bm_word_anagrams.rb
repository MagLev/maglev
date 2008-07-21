require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_word_anagrams.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # Word anagrams (with duplicates).
  # Submitted by Giovanni Intini <intinig@gmail.com>
  
  class String
    def swap(i)
      tmp = self[0,1]
      self[0] = self[i]
      self[i] = tmp
      self
    end
    
    def permutations
      return [self] if size == 1
  
      results = []
      tmp = self
      size.times do |pos|
        tmp.swap(pos)
        partial_results = tmp[1..-1].permutations
        partial_results.each_index do |i|
          partial_results[i] = tmp[0,1] + partial_results[i]
        end
        results << partial_results
      end
      results.flatten
    end
  end 
  
  puts "alongword".permutations.size

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_word_anagrams.rb'
