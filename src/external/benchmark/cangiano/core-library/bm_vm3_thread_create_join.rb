require 'benchmark'
puts '==> Start benchmark: ./core-library/bm_vm3_thread_create_join.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i=0
  while i<1000 # benchmark loop 3
    i+=1
    Thread.new{
    }.join
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-library/bm_vm3_thread_create_join.rb'
