# This crash is more specific than make_crash2.
require 'lib/treekd'

p1 = Collections::Point2D.new(0, 1, 'test1')
p2 = Collections::Point2D.new(0, 2, 'test2')
tree = Collections::Tree2D.new [p1, p2]
tree.print_tree


# Emulate the behavior of Maglev's Enumerable class
# to do a find()
#result = tree.find { |val| val.data == 'test3' }
result = tree.each { |o|
  return o if o.data == 'test3'
}

puts "result: #{result}"
