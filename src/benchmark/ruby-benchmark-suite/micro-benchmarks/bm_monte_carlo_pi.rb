# Monte Carlo Pi
# Submitted by Seo Sanghyeon
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
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
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
