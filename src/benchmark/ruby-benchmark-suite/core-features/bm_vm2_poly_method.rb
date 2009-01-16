require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

class C1
  def m
    1
  end
end

class C2
  def m
    2
  end
end

input_sizes = [1_000_000, 2_000_000, 4_000_000, 8_000_000]

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    o1 = C1.new
    o2 = C2.new
    n.times do |i|
      o = (i % 2 == 0) ? o1 : o2
      o.m; o.m; o.m; o.m; o.m; o.m; o.m; o.m
    end
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
