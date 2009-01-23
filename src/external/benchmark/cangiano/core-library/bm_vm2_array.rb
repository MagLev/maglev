require 'benchmark'
puts '==> Start benchmark: ./core-library/bm_vm2_array.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i=0
  while i<6000000 # benchmark loop 2
    i+=1
    a = [1,2,3,4,5,6,7,8,9,10]
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-library/bm_vm2_array.rb'
