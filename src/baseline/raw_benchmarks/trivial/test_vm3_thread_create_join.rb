[1000, 10_000, 100_000].map do |n|
  n.times { Thread.new{}.join }
end
