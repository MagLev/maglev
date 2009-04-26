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
