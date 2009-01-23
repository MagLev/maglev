require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_loop_whileloop.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i = 0
  while i<30000000 # benchmark loop 1
    i+=1
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_loop_whileloop.rb'
