# from ruby 1.9 benchmarks, slightly modified.
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

benchmark = BenchmarkRunner.new(label, iterations, timeout)
Const = 1 
benchmark.run do

i = 0 
while i<3000000
  i+= Const 
  j = Const 
  k = Const 
end 

end

File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
