require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_monte_carlo_pi.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # Monte Carlo Pi
  # Submitted by Seo Sanghyeon
  
  sample = 10_000_000
  count = 0
  
  sample.times do
    x = rand
    y = rand
    if x * x + y * y < 1
      count += 1
    end
  end
  
  pi = 4.0 * count / sample
  puts pi

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_monte_carlo_pi.rb'
