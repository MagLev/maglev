# test context switching between two threads with very different stack sizes
# from the bogus2.rb file
# of the Brent Roman MBARI patches 
# http://sites.google.com/site/brentsrubypatches/
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last



def outer name
  ball = Thread.current
  inner = Thread.new Thread.current do | outer |
    loop {
      Thread.stop until (Thread.critical=true; ball == inner)
      (ball=outer).run
    }
  end
  2000.times {|i|  
    Thread.stop until (Thread.critical=true; ball == Thread.current)
    (ball=inner).run
  }
end

def recurse someArg
  if ($depth+=1) < 2000
    recurse 3.14159
  else
    outer :outer
  end
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  $depth = 0
  recurse "top"
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
