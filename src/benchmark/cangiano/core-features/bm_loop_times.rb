require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_loop_times.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  30000000.times{|e|}

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_loop_times.rb'
