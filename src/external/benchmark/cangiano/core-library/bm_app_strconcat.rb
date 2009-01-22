require 'benchmark'
puts '==> Start benchmark: ./core-library/bm_app_strconcat.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i=0
  while i<500000
    "#{1+1} #{1+1} #{1+1}"
    i+=1
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-library/bm_app_strconcat.rb'
