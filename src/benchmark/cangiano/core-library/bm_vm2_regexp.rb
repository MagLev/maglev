require 'benchmark'
puts '==> Start benchmark: ./core-library/bm_vm2_regexp.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i=0
  str = 'xxxhogexxx'
  while i<6000000 # benchmark loop 2
    /hoge/ =~ str
    i+=1
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-library/bm_vm2_regexp.rb'
