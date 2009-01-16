require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def rescue_loop(n)
  n.times do
    begin
    rescue
    end
  end
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  rescue_loop(1_000_000)
end

File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
