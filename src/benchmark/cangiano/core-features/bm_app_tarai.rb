require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_app_tarai.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  def tarai( x, y, z )
    if x <= y
    then y
    else tarai(tarai(x-1, y, z),
               tarai(y-1, z, x),
               tarai(z-1, x, y))
    end
  end
  
  tarai(12, 6, 0)

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_app_tarai.rb'
