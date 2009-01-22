require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_app_fib.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  def fib(n)
    if n < 2
      n
    else
      fib(n-1) + fib(n-2)
    end
  end
  
  35.times {|n| puts fib(n) }
  

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_app_fib.rb'
