# Lucas–Lehmer primality test for Mersenne numbers (2**p - 1)
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def is_prime?(p)
  s = 4
  m = 2**p - 1
  (p-2).times do
    s = (s**2 - 2) % m
  end
  s == 0 ? true : false
end

input_sizes = [9689, 9941, 11213, 19937] # 4 Mersenne's exponents

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    puts "2**#{p} - 1 is prime" if is_prime?(n)
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end


