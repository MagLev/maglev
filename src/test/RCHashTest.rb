require File.expand_path('simple', File.dirname(__FILE__))

require 'maglev/rchash'

# Test condition of an empty rchash
rchash = RCHash.new
assert_not_nil(rchash, 'RCHash.new')
test(rchash.size, 0, 'size of empty rchash')
test(rchash.empty?, true, 'empty? on empty rchash')

keys = rchash.keys
assert_not_nil(keys, 'RCHash.keys should not be nil')
assert_type(keys, Array, 'RCHash.keys should return an Array')
test(keys.size, 0, 'Expecting no keys')

# Test basic put/get/delete of key

rchash['foo'] = 10
test(rchash['foo'], 10, "['foo'] => 10")
test(rchash.empty?, false, 'empty? on non-empty rchash')
test(rchash.delete("foo"), 10, "delete 'foo' => 10")
test(rchash.empty?, true, 'empty? on newly empty rchash')

# access of uninitialized key is nil
rchash = RCHash.new
test(rchash[:x], nil, "rchash[:x] should be nil")  # divide by zero!

10.times { |i| rchash[i] = i }
sum = 0
rchash.each { |k,v| sum += v }
test(sum, 45, 'each')

report
