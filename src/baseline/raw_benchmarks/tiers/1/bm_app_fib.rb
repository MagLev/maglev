def fib(n)
  if (n < 2)
    n
  else
    fib(n-1) + fib(n-2)
  end
end

def Bench.run
  fib(35)
end
