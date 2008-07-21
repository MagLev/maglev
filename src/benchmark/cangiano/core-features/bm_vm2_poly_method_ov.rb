require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm2_poly_method_ov.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  class C1
    def m
      1
    end
  end
  class C2
    def m
      2
    end
  end
  
  o1 = C1.new
  o2 = C2.new
  
  i=0
  while i<6000000 # benchmark loop 2
    o = (i % 2 == 0) ? o1 : o2
  #  o.m; o.m; o.m; o.m; o.m; o.m; o.m; o.m
    i+=1
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm2_poly_method_ov.rb'
