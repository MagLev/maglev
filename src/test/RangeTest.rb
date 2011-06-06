require File.expand_path('simple', File.dirname(__FILE__))

r = ('A'..'D')
results = []
r.each { |i| results << i }

test(results, ['A', 'B', 'C', 'D'], "('A'..'D') test")

r = 1..3
test( r.last , 3, "last")

r = Range.new(1,5)
test( r.to_a, [ 1,2,3,4,5] , "to_a2")

# Test GemStone internal method
[ (12..-1), (20..25), (20..1), (-20..1), (-20..-1)].each do |r|
  test(r.__beg_len(11), nil, "A: expect nil for #{r.inspect}")
end

[(-1..-1), (-1...-1), (-1..0), (-1...0) ].each do |r|
  test(r.__beg_len(0), nil, "B: expect nil for #{r.inspect}")
end

# Some tests the are in range
test((1..1).__beg_len(11), [1,1],   "C: __beg_len ")
test((5..-1).__beg_len(11), [5,6],  "D: __beg_len ")
test((5...-1).__beg_len(11), [5,5], "E: __beg_len ")

# uses from sinatra unit tests
unless (nil === (4..6)).equal?(false) ; raise 'fail'; end
unless (true === (4..6)).equal?(false) ; raise 'fail'; end
unless ((4..6) === true ).equal?(false) ; raise 'fail'; end
unless ((4..6) === nil).equal?(false) ; raise 'fail'; end

report
