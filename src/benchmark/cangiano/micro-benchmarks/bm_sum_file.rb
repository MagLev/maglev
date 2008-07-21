require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_sum_file.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # Adapted for the Ruby Benchmark Suite.
  
  100.times do
    count = 0
    File.open("random.input", "r").each_line do |line|
      count += line.to_i
    end
    puts count
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_sum_file.rb'
