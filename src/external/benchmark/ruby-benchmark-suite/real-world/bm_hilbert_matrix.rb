# Submitted by M. Edward (Ed) Borasky
require File.dirname(__FILE__) + '/../lib/benchutils'
require 'hilbert' # also brings in mathn, matrix, rational and complex

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def run_hilbert(dimension)
    m = hilbert(dimension)
    print "Hilbert matrix of dimension #{dimension} times its inverse = identity? "
    k = m * m.inv
    print "#{k==Matrix.I(dimension)}\n"
    m = nil # for the garbage collector
    k = nil
end

input_sizes = [10, 20, 30, 40, 50, 60]

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    run_hilbert(n)
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
