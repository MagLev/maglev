# from http://www.bagley.org/~doug/shootout/bench/sieve/sieve.ruby
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = 600 # ARGV[-2].to_i
report = ARGV.last

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  num = 4000
  count = i = j = 0
  flags0 = Array.new(8192,1)
  k = 0
  while k < num
    k+=1
    count = 0
    flags = flags0.dup
    i = 2
    while i<8192
      i+=1
      if flags[i]
        # remove all multiples of prime: i
        j = i*i
        while j < 8192
          j += i
          flags[j] = nil
        end
        count += 1
      end
    end
  end
  puts count
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
