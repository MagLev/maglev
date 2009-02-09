# this one counts to 1_000_000 divided among threads
# i.e. 4 threads => each counts to 250_000

require File.dirname(__FILE__) + '/../lib/benchutils'

def count_high how_many
  how_many.times {}
end

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")

iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

input_sizes = [1, 2, 4, 8, 16]

# For some benchmarks it doesn't even make sense to have variable input sizes.
# If this is the case, feel free to remove the outer block that iterates over the array.
input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    threads = []
    n.times{ threads << Thread.new { count_high(1_000_000 / n) }}
    threads.each{|t| t.join}
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
