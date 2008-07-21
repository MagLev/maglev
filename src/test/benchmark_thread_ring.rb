# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_thread_ring.rb
# Not expected to succeed - Thread implementation incomplete.
#

require 'benchmark'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_thread_ring '){
    require 'thread'
  
    THREAD_NUM = 503
    number = (ARGV.first || 1_000_000).to_i
  
    threads = []
    for i in 1..THREAD_NUM
      threads << Thread.new(i) do |thr_num|
        while true
          Thread.stop
          if number > 0
            number -= 1
          else
            puts thr_num
            exit 0
          end
        end
      end
    end
  
    prev_thread = threads.last
    while true
      for thread in threads
        Thread.pass until prev_thread.stop?
        thread.run
        prev_thread = thread
      end
    end
  }
  
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
