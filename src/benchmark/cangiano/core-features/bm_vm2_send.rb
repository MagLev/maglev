require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_vm2_send.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  class C
    def m
    end
  end
  
  o = C.new
  
  i=0
  while i<6000000 # benchmark loop 2
    i+=1
    o.__send__ :m
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_vm2_send.rb'
