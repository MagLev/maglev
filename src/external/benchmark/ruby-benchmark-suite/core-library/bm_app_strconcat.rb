require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  1_500_000.times { "#{1+1} #{1+1} #{1+1}" }
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
