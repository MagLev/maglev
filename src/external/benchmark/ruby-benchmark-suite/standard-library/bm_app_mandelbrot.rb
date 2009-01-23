require File.dirname(__FILE__) + '/../lib/benchutils'
require 'complex'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def mandelbrot?(z)
  i = 0
  while i<100
    i+=1
    z = z * z
    return false if z.abs > 2
  end
  true
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  ary = []

  (0..100).each do |dx|
    (0..100).each do |dy|
      x = dx / 50.0
      y = dy / 50.0
      c = Complex(x, y)
      ary << c if mandelbrot?(c)
    end
  end
  p ary
end
 
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
