def count_high
  100_000.times {}
end

[1, 2, 4, 8, 16].map do |n|
  threads = []
  n.times{ threads << Thread.new { count_high }}
  threads.each{|t| t.join}
end
