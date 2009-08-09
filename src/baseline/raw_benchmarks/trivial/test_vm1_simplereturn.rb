def method1
  return 1
end

[10_000_000, 20_000_000, 30_000_000].map do |n|
  n.times { method1 }
end
