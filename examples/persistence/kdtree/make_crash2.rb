require 'lib/tree2d'

p1 = Collections::Point2D.new(0, 1, 'test1')
p2 = Collections::Point2D.new(0, 2, 'test2')
tree = Collections::Tree2D.new [p1, p2] # p2 is in the root, p1's node falls to the left of the root

# Emulate the behavior of Maglev's Enumerable class
# to do a find().
# The RuntimeError is a byproduct of not being able to
# discover 'test3', a non-existent element in the 
# tree.
# More specifically, I have done some puts-style debugging 
# in TreeKD and here is what happens inside of tree.each that 
# through some means causes the RuntimeError:
#   - Inside of the root of the tree, we descend down the 
#     left of the tree and make a successfull call to its
#     each().
#   - Still in the root node's each(), we next execute the
#     second line and get a RuntimeError.  I have narrowed
#     this down to making any reference to the "block"
#     variable.  It gets confusing here, because otherwise
#     this sort of tree descent works normally.  There
#     is something going on with the find() implementation
#     below that interacts with each() to (I assume)
#     generate bad bytecode in the ruby compiler.

#result = tree.find { |val| val.data == 'test3' }
result = tree.each { |o|
  return o if o.data == 'test3'
}

puts "result: #{result}"
