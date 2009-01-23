require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_startup.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  1 + 1

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_startup.rb'
