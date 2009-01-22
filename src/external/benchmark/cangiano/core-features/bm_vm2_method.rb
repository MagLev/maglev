require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm2_method.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  def m
    nil
  end
  
  i=0
  while i<6000000 # benchmark loop 2
    i+=1
    m; m; m; m; m; m; m; m;
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm2_method.rb'
