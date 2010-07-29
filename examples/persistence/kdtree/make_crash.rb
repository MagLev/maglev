require 'lib/treekd'

p1 = Collections::Point2D.new(0, 1, 'test1')
p2 = Collections::Point2D.new(0, 2, 'test2')
tree = Collections::Tree2D.new [p1, p2]

# Note: The RuntimeError is triggered simply by search
# for something not in the list.
result = tree.find { |val| val.data == 'test3' }

puts "result: #{result}"
