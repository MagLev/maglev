def fib(n)
  if (n < 2)
    n
  else
    fib(n-1) + fib(n-2)
  end
end

[30, 35].map do |n|
  fib(n)
end
