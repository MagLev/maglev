# 
# Benchmark block of code from ruby-benchmark-suite/core-library/bm_so_count_words.rb
#

require 'benchmark'

Benchmark.bm(12) do |bmr|
  
  bmr.report('bm_so_count_words '){
    500.times do

      input = open(File.join(File.dirname($0), 'wc.input'), 'rb')

      nl = nw = nc = 0
      while true
        data = (input.read(4096) or break) << (input.gets || "")
        nc += data.length
        nl += data.count("\n")
        ((data.strip! || data).tr!("\n", " ") || data).squeeze!
        #nw += data.count(" ") + 1
      end

      input.close

    end
    # STDERR.puts "#{nl} #{nw} #{nc}"
  }  

end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
