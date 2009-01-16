require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def m
  yield
end

input_sizes = [100_000, 1_000_000, 10_000_000]

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    n.times { m {} }
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
