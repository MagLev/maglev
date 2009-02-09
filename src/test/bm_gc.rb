# 
# Benchmark blocks of code selected to stress VM garbage collection.
#
# When both of these benchmarks run are run sequentially (as in this file) the 
# MagLev gem needs at least 350MB gemstone GEM_TEMPOBJ_CACHE_SIZE to run this
#

require 'benchmark'

Benchmark.bm(7) do |bmr|
  
#  Run a block 10 times, where the block creates 
#    40MB(32bit VM) or 80MB(64bit VM) of 100 element Arrays
#    and 10 times that much garbage.
#    Needs 150MB gemstone GEM_TEMPOBJ_CACHE_SIZE"
  bmr.report("gc_arrays       "){
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
  }
  
#  Run a block 10 times, where the block creates 
#   10MB of 1K strings and 10 times that much garbage
#   Needs at least 250MB gemstone GEM_TEMPOBJ_CACHE_SIZE
  bmr.report("gc_strings      "){
    a = nil
    10.times do |i|
      a = Array.new(10000)
      a.length.times do |x|
        str = ""
        20.times{|j| str << "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYy"}
        a[x] = str
        10.times do |y|
          junk = ""
          20.times{|k| junk << "GaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgE_"}
        end
      end
    end
  }
  
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil

