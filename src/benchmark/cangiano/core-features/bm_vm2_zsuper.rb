require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm2_zsuper.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i = 0
  
  class C
    def m a
      1
    end
  end
  
  class CC < C
    def m a
      super
    end
  end
  
  obj = CC.new
  
  while i<6000000 # benchmark loop 2
    obj.m 10
    i+=1
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm2_zsuper.rb'
