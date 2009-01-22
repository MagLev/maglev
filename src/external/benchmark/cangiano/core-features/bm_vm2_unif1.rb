require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm2_unif1.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i = 0
  def m a, b
  end
  
  while i<6000000 # benchmark loop 2
    i+=1
    m 100, 200
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm2_unif1.rb'
