
10.times do |i|
  x = rand(100)
  p x
end
puts "Commit txn"
Gemstone.commitTransaction
10.times do |i|
  x = rand(100)
  p x
end
