# from http://www.bagley.org/~doug/shootout/bench/random/random.ruby
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def gen_random(max, last, im, ia, ic)
  (max * (last = (last * ia + ic) % im)) / im
end

input_sizes = [100_000, 500_000, 1_000_000]

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    n.times{ gen_random(100.0, 42.0, 139968.0, 3877.0, 29573.0) }
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
