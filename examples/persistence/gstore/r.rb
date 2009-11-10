
10.times do |i|
  x = rand(100)
  p x
end
puts "Commit txn"
Maglev.commit_transaction
10.times do |i|
  x = rand(100)
  p x
end
