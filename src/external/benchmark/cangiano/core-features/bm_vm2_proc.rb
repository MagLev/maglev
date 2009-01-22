require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm2_proc.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  def m &b
    b
  end
  
  pr = m{
    a = 1
  }
  
  i=0
  while i<6000000 # benchmark loop 2
    i+=1
    pr.call
  end
  

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm2_proc.rb'
