# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_nsieve.rb
# PASSES

require 'benchmark'

Benchmark.bm(7) do |bmr|
 
  bmr.report('bm_nsieve '){
    def sieve(m)
      flags = Flags.dup[0,m]
      count = 0
      pmax = m - 1
      p = 2
      while p <= pmax
        unless flags[p].zero?
          count += 1
          mult = p
          while mult <= pmax
            flags[mult] = 0
            mult += p
          end
        end
        p += 1
      end
      count
    end
  
    n = (ARGV[0] || 9).to_i
  
    Flags = "\x1" * ( 2 ** n * 10_000)
  
    n.downto(n-2) do |exponent|
      break if exponent < 0
      m = (1 << exponent) * 10_000
      # m = (2 ** exponent) * 10_000
      count = sieve(m)
      # printf "Primes up to %8d %8d\n", m, count
    end
  
  }
 
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
