# $Id: nestedloop-ruby.code,v 1.4 2004/11/13 07:42:22 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from Avi Bryant
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def nested_loop(n)
  x = 0
  n.times do
    n.times do
      n.times do
        n.times do
          n.times do
            n.times do
              x += 1
            end
          end
        end
      end
    end
  end
  return x
end

input_sizes = [5, 10, 15]

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    puts nested_loop(n)
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end



