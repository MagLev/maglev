require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_app_tak.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  
  def tak x, y, z
    unless y < x
      z
    else
      tak( tak(x-1, y, z),
           tak(y-1, z, x),
           tak(z-1, x, y))
    end
  end
  
  tak(18, 9, 0)
  

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_app_tak.rb'
