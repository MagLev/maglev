# This crash is more specific than make_crash4.
require 'lib/treekd'

class TestValue
  def initialize(code)
    @zip = code
  end

  def zip
    @zip
  end
end

class NodeTest
  def initialize(val, left, right)
    @val, @left, @right = val, left, right
  end

  def each(&block)
    @left.each(&block) unless @left.nil?
    block.call(val) if @value and block
    @right.each(&block) unless @right.nil?
  end
end

p1 = Collections::Point2D.new(0, 1, 'test1')
p2 = Collections::Point2D.new(0, 2, 'test2')
tree = Collections::Tree2D.new [p1, p2]
tree.print_tree
#leftNode = NodeTest.new(TestValue.new(72701), nil,      nil)
#rootNode = NodeTest.new(TestValue.new(72702), leftNode, nil)

# Emulate the behavior of Maglev's Enumerable class
# to do a find()
#result = tree.find { |val| val.data == 'test3' }
result = rootNode.each { |o|
  return o if o.data == 'test3'
}

puts "result: #{result}"
