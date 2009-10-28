[[10.0, "10.0"],
 [232.3, "232.3"],
 [3.121999, "3.121999"]].each do |info|
  actual = info[0].to_s
  expected = info[1]
  raise "Failed on #{expected}: #{actual}" unless expected == actual
end
