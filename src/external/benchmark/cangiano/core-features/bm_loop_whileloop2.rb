require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_loop_whileloop2.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i=0
  while i<60000000 # benchmark loop 2
    i+=1
  end
  

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_loop_whileloop2.rb'
