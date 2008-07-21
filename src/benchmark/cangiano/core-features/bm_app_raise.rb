require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_app_raise.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  i=0
  while i<300000
    i+=1
    begin
      raise
    rescue
    end
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_app_raise.rb'
