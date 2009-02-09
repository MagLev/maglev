require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
  100.times do
    count = 0
    fname = File.dirname(__FILE__) + "/random.input"
    File.open(fname, "r").each_line do |line| 
      count += line.to_i
    end
    puts count
  end
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
