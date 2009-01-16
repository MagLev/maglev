require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

input_sizes = [10, 100, 1000, 10_000]

input_sizes.each do |n|
  string = "a" * n
  array = Array.new(n, 42)
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    30_000_000.times do
      string.length
      array.length
    end
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
