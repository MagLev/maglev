#  There was a bug where doing an abort would reset the seed, so if you
#  were in a loop and doing an abort, you'd get the same random number all
#  the time.
require File.expand_path('simple', File.dirname(__FILE__))

results = Hash.new(1)
1000.times do
  results[rand(100)] += 1
end
raise "Fail A" unless results.size > 5

results = Hash.new(1)
1000.times do
  results[rand(100)] += 1
  Maglev.abort_transaction
end
raise "Fail B" if results.size < 5

# Test srand
srand(12345)
test(rand(100), 41, "First random after srand")
test(rand(100), 11, "Second random after srand")
test(rand(100), 84, "Third random after srand")

srand(12345)
test(rand(100), 41, "First random after srand (2)")
test(rand(100), 11, "Second random after srand (2)")
test(rand(100), 84, "Third random after srand (2)")

# Trac 548: rand(0) was throwing an exception.
test(rand(0).kind_of?(Float), true, 'Trac 548: should be a float')

report
