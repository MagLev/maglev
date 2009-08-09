def m(&b)
  b
end

[1_000_000, 2_000_000, 4_000_000, 8_000_000].map do |n|
  pr = m { a = 1 }
  n.times do |i|
    pr.call
  end
end
