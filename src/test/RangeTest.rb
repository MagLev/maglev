require File.expand_path('simple', File.dirname(__FILE__))

r = ('A'..'D')
results = []
r.each { |i| results << i }

test(results, ['A', 'B', 'C', 'D'], "('A'..'D') test")

r = 1..3
test( r.last , 3, "last")

r = Range.new(1,5)
test( r.to_a, [ 1,2,3,4,5] , "to_a2")


report
