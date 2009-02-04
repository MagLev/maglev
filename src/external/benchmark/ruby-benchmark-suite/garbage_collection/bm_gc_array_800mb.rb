# This is required for every benchmark
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")

iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last


# start test:

#  Run a block 10 times, where the block creates 
#    40MB(32bit VM) or 80MB(64bit VM) of 100 element Arrays
#    and 10 times that much garbage.
#    Needs 150MB gemstone GEM_TEMPOBJ_CACHE_SIZE"

  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    a = nil
    10.times do |i|
      a = Array.new(100000)
      a.length.times do |x|
        a[x] = Array.new(100)
        10.times do |y|
          Array.new(100)
        end
      end
    end
    #  Prevent saving this into RubyContext when running under topaz>maglev
    Gemstone.abortTransaction rescue nil
  end

  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
