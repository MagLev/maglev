require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm1_length.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  a = 'abc'
  b = [1, 2, 3]
  i=0
  while i<30000000 # while loop 1
    i+=1
    a.length
    b.length
  end
  

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm1_length.rb'
