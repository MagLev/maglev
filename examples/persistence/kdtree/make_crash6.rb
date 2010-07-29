# This crash is more specific than make_crash6.
require 'lib/treekd'

p1 = Collections::Point2D.new(0, 1, 'test1')
p2 = Collections::Point2D.new(0, 2, 'test2')
p3 = Collections::Point2D.new(3, 3, 'test3')
tree = Collections::Tree2D.new [p1, p2, p3]
tree.print_tree

# Emulate the behavior of Maglev's Enumerable class
# to do a find()
#result = tree.find { |val| val.data == 'test3' }
result = tree.each { |o|
  return o if o.data == 'test4'
}

#def foo_each(array, &block) 
#  array.each { |o|
#    return o if block.call(o)
#  }
#end



result = nil

