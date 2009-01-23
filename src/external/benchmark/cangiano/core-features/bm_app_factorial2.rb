require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_app_factorial2.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # Reduced version for MRI
  
  def fact(n)
    if(n > 1)
      n * fact(n-1)
    else
      1
    end
  end
  
  5.times do 
    puts fact(3000)
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_app_factorial2.rb'
