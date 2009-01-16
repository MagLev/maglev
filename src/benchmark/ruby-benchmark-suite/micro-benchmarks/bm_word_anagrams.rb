# Word anagrams (with duplicates).
# Submitted by Giovanni Intini <intinig@gmail.com>
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

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

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  puts "alongword".permutations.size
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
