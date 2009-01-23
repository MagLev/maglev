require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_recursive.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # ----------------------------------------------------------------------
  # The Computer Language Shootout
  # http://shootout.alioth.debian.org/
  #
  # Code based on / inspired by existing, relevant Shootout submissions
  #
  # Contributed by Anthony Borla
  # Optimized by Jesse Millikan
  # ----------------------------------------------------------------------
  
  def ack(m, n)
    if m == 0
      n + 1
    elsif n == 0
      ack(m - 1, 1)
    else
      ack(m - 1, ack(m, n - 1))
    end
  end
  
  # ---------------------------------
  
  def fib(n)
     if n > 1
       fib(n - 2) + fib(n - 1)
     else
       n
     end
  end
  
  # ---------------------------------
  
  def tak(x, y, z)
    if y < x
     tak(tak(x - 1.0, y, z), tak(y - 1.0, z, x), tak(z - 1.0, x, y))
    else 
      z
    end
  end
  
  # ---------------------------------
  
  n = (ARGV.shift || 8).to_i
  
  printf("Ack(3,%d): %d\n", n, ack(3, n))
  printf("Fib(%.1f): %.1f\n", 27.0 + n, fib(27.0 + n))
  
  n -= 1
  printf("Tak(%d,%d,%d): %d\n", n * 3, n * 2, n, tak(n * 3, n * 2, n))
  
  printf("Fib(3): %d\n", fib(3))
  printf("Tak(3.0,2.0,1.0): %.1f\n", tak(3.0, 2.0, 1.0))
  

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_recursive.rb'
