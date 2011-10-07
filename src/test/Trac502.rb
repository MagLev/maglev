require File.expand_path('simple', File.dirname(__FILE__))

# 1 39  96 13 5
# 1 39  96 10 5
# 1 39  98 10 4
# 1 39  99  8 4
# 1 39 114  3 6

a = [1, 2, 3, 4, 5]
a.fill('x', -2)
test(a, [1, 2, 3, 'x', 'x'], 'fill 1')

a = [1, 2, 3, 4, 5]
a.fill('x', -2, nil)
test(a, [1, 2, 3, 'x', 'x'], 'fill 1')

a = [1, 2, 3, 4, 5]
a.fill('x', -2)
test(a, [1, 2, 3, 'x', 'x'], 'fill 3')

a = [1, 2, 3, 'x', 'x']
a.fill(-2) {|i| i.to_s}
test(a, [1, 2, 3, '3', '4'], 'fill 4')

a = [1, 2, 3, 4, 5, 6]
a.fill('x', -2..0)
test(a, [1, 2, 3, 4, 5, 6], 'fill 5')

a = [1, 2, 3, 4, 5, 6]
a.fill('x', 0...0)
test(a, [1, 2, 3, 4, 5, 6], 'fill 6')

a = [1, 2, 3, 4, 5, 6]
a.fill('x', -4..4)
test(a, [1, 2, 'x', 'x', 'x', 6], 'fill 7')

a = [1, 2, 3, 4, 5, 6]
a.fill('x', -4...4)
test(a, [1, 2, 'x', 'x', 5, 6], 'fill 8')

a = [1, 2, 3, 4, 5, 6]
a.fill(-4..4){|i| (i+1).to_s}
test(a, [1, 2, '3', '4', '5', 6], 'fill 9')

a = [1, 2, 3, 4, 5, 6]
a.fill(-4...4){|i| (i+1).to_s}
test(a, [1, 2, '3', '4', 5, 6], 'fill 10')

report
#################### Trac Info
# ID:         502
# Summary:    Array#fill not handling negative offsets
# Changetime: 2009-04-26 19:26:33+00:00
###

#  From the documentation:
#  
#  array.fill(obj) → array
#  array.fill(obj, start [, length]) → array
#  array.fill(obj, range ) → array
#  array.fill {|index| block } → array
#  array.fill(start [, length] ) {|index| block } → array
#  array.fill(range) {|index| block } → array
#  The first three forms set the selected elements of self (which may be the entire array) to obj. A start of nil is equivalent to zero. A length of nil is equivalent to self.length. The last three forms fill the array with the value of the block. The block is passed the absolute index of each element to be filled.
#  
#     a = [ "a", "b", "c", "d" ]
#     a.fill("x")              #=> ["x", "x", "x", "x"]
#     a.fill("z", 2, 2)        #=> ["x", "x", "z", "z"]
#     a.fill("y", 0..1)        #=> ["y", "y", "z", "z"]
#     a.fill {|i| i*i}         #=> [0, 1, 4, 9]
#     a.fill(-2) {|i| i*i*i}   #=> [0, 1, 8, 27]