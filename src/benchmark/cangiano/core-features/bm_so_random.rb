require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_so_random.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # from http://www.bagley.org/~doug/shootout/bench/random/random.ruby
  
  IM = 139968.0
  IA = 3877.0
  IC = 29573.0
  
  $last = 42.0
  
  def gen_random(max)
    (max * ($last = ($last * IA + IC) % IM)) / IM
  end
  
  N = 1000000
  
  i=0
  while i<N
    i+=1
    gen_random(100.0)
  end
  puts "%.9f" % gen_random(100.0)

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_so_random.rb'
