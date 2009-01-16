# $Id: ary-ruby.code,v 1.4 2004/11/13 07:41:27 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Paul Brannan and Mark Hubbart
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

n = 9000

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  x = Array.new(n)
  y = Array.new(n, 0)

  n.times do |bi|
    x[bi] = bi + 1
  end

  (0 .. 999).each do |e|
    (n-1).step(0,-1) do |bi|
      y[bi] += x.at(bi)
    end
  end
  
  puts "#{y.first} #{y.last}"
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
