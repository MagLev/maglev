require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm2_super.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  
  class C
    def m
      1
    end
  end
  
  class CC < C
    def m
      super()
    end
  end
  
  obj = CC.new
  
  i = 0
  while i<6000000 # benchmark loop 2
    obj.m
    i+=1
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm2_super.rb'
