require File.expand_path('simple', File.dirname(__FILE__))

require 'maglev/rcqueue'

# Test condition of an empty rcqueue
q = RCQueue.new
assert_not_nil(q, 'RCQueue.new')
test(q.size, 0, 'size of empty q')
test(q.length, 0, 'length of empty q')
test(q.empty?, true, 'empty? on empty q')

# Test basic addition

test(q.first, nil, "first value when empty")
q << 10
test(q.empty?, false, 'empty? on non-empty q')
test(q.first, 10, "first value")
q.enq 10
q.push 10
test(q.size, 3, 'size after adding')
test(q.deq, 10, 'deq retrieves first element')
test(q.size, 2, 'deq removes first element')
q << 1
test(q.clear, [10, 10, 1], 'clear returns elements as array')
q << 1
q << 2
q << 3
test(q.pop, 1, 'pop retrieves first element')
test(q.size, 2, 'pop removes first element')
test(q.shift, 2, 'shift retrieves first element')
test(q.size, 1, 'shift removes first element')
test(q.first, 3, 'check state after shifting & popping')
test(q.clear, [3], 'clear returns elements as array')
test(q.empty?, true, 'clear empties queue')

sum_it = RCQueue.new
10.times { |i| sum_it << i }
sum = 0
sum_it.each { |v| sum += v }
test(sum, 45, 'each')

report
