def m
  yield
end

[100_000, 1_000_000, 10_000_000].map do |n|
  n.times { m {} }
end
