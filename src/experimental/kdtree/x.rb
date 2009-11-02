require 'point'
require 'tree2d'

a = Point.new( 0, -2, :a)
b = Point.new(-1,  1, :b)
c = Point.new( 1,  0, :c)
d = Point.new(-3,  2, :d)
e = Point.new(-2, -1, :e)
f = Point.new( 3,  2, :f)
g = Point.new( 2, -1, :g)
# tree = Tree2D.new [a, b, c, d, e, f, g]
# in_order_traversal = [d, b, e, a, f, c, g]
# # tree.each { |el| el.value.must_equal depth_first_order.shift }
# tree.each { |el| p el }


points = [b, g, d, e, a, c, f]
axis = 0
sorted = points.sort {|a,b| a[axis] <=> b[axis] }
pivot = sorted.size / 2

puts sorted.join("\n")
p pivot

depth = 0
left = Tree2D.new(sorted[0...pivot], depth+1) if pivot > 0
right = Tree2D.new(sorted[pivot+1..-1], depth+1) if pivot > 0

puts "LEFT"
sorted[0...pivot].sort {|a,b| a[1] <=> b[1]}.each {|el| p el}
