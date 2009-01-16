# $Id: strcat-ruby.code,v 1.4 2004/11/13 07:43:28 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# based on code from Aristarkh A Zagorodnikov and Dat Nguyen
# Modified for the Ruby Benchmark Suite.
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

input_sizes = [5000, 10_000, 15_000]

input_sizes.each do |n|
  base = "a" * n
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    string = ''
    300_000.times do
      string << base
    end
    puts string.length
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
