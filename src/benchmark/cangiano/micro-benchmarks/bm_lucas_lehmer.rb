require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_lucas_lehmer.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # Lucas–Lehmer primality test for Mersenne numbers (2**p - 1)
  
  def is_prime?(p)
    s = 4
    m = 2**p - 1
    (p-2).times do
      s = (s**2 - 2) % m
    end
    s == 0 ? true : false
  end
  
  # Exponents for the first 25 Mersenne Primes
  exponents = [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279,
               2203, 2281, 3217, 4253, 4423, 9689, 9941, 11213, 19937, 21701]
               
  n = (ARGV[0] || 21).to_i
  
  # Exponent of the nth Mersenne Prime
  p = exponents[n-1]
  
  puts "2**#{p} - 1 is prime" if is_prime?(p) 

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_lucas_lehmer.rb'
