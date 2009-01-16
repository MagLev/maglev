require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def raise_and_rescue(n)
  n.times do
    begin
      raise
    rescue
    end
  end
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  raise_and_rescue(300_000)
end

File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
