# This is required for every benchmark
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")

iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last


# start test:

#  Run a block 10 times, where the block creates 
#   5MB of 1K strings and 10 times that much garbage
#   Needs at least 250MB gemstone GEM_TEMPOBJ_CACHE_SIZE

  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    a = nil
    10.times do |i|
      a = Array.new(5000)
      a.length.times do |x|
        str = ""
        20.times{|j| str << "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYy"}
        a[x] = str
        10.times do |y|
          junk = ""
          20.times{|k| str << "GaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgE_"}
        end
      end
    end
    #  Prevent saving this into RubyContext when running under topaz>maglev
    Gemstone.abortTransaction rescue nil
  end

  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
