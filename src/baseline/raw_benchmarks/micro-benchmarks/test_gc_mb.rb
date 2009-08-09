[500_000, 1_000_000, 3_000_000].map do |n|
  a = []
  n.times { a << []} # use up some RAM
  n.times {[]}
end
