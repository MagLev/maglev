# test context switching between two threads with very different stack sizes
# from Brent Roman MBARI patches  bogus1.rb file
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def outer name
  if ($depth+=1) < 2000
    outer name
  else
    250.times {
      Thread.new do
        inner :inner, name, Thread.current
      end.join
    }
  end
end

def inner innerName, outerName, parent
  Thread.new do
    parent.join
    k = Proc.new {|n0, n1| q = n0.to_s << n1.to_s }
    k[innerName, outerName]
  end
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  $depth = 0
  outer :outer
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
