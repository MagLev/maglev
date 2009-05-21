#  There was a bug where doing an abort would reset the seed, so if you
#  were in a loop and doing an abort, you'd get the same random number all
#  the time.

results = Hash.new(1)
1000.times do
  results[rand(100)] += 1
end
raise "Fail A" unless results.size > 5

results = Hash.new(1)
1000.times do
  results[rand(100)] += 1
  Gemstone.abortTransaction
end
raise "Fail B" if results.size < 5

