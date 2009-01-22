# The Computer Language Shootout
# http://shootout.alioth.debian.org/
# Contributed by Sokolov Yura
# Modified by Ryan Williams
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = 600 # ARGV[-2].to_i
report = ARGV.last

def fannkuch(n)
   maxFlips, m, r, check = 0, n-1, n, 0
   count = (1..n).to_a
   perm = (1..n).to_a

   while true
      if check < 30
         #puts "#{perm}"
         check += 1
      end

      while r != 1
         count[r-1] = r
         r -= 1
      end

      if perm[0] != 1 and perm[m] != n
         perml = perm.clone #.dup
         flips = 0
         while (k = perml.first ) != 1
            perml = perml.slice!(0, k).reverse + perml
            flips += 1
         end
         maxFlips = flips if flips > maxFlips
      end
      while true
         return maxFlips if r == n
         perm.insert r,perm.shift
         break if (count[r] -= 1) > 0
         r += 1
      end
   end
end

input_sizes = [6, 8, 10]

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    puts "Pfannkuchen(#{n}) = #{fannkuch(n)}"
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
