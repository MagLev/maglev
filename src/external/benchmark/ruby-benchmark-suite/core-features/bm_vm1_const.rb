require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

n = 500_000
iter = 0

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  iter +=1 
  puts iter
  n.times do |i|
    Object.const_set("CONST#{iter}#{i}", 1)
    const = Object.const_get("CONST#{iter}#{i}")
  end
end

File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
