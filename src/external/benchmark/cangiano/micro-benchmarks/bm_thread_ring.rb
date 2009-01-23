require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_thread_ring.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # The Computer Language Benchmarks Game
  # http://shootout.alioth.debian.org/
  #
  # contributed by Serhiy Boiko
  
  
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

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_thread_ring.rb'
