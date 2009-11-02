# Tree2D is a KD-Tree of dimension 2.
#
# The points given to a tree must have the following methods:
#   cmp(axis, other)
#
#
# TODO: Instead of checking axis all the time, make subclasses for each axis?
class Tree2D
  attr_reader :left, :right, :value
  def initialize(points, depth=0)
    @axis = depth % 2
    return if points.nil? or points.empty?

    sorted = points.sort {|a,b| a[@axis] <=> b[@axis] }
    pivot = sorted.size / 2
    #puts "pivot: #{pivot} sorted: #{sorted.inspect}"
    @left = Tree2D.new(sorted[0...pivot], depth+1) if pivot > 0
    @right = Tree2D.new(sorted[pivot+1..-1], depth+1) if pivot > 0
    @value = sorted[pivot]
  end

  # Does an in-order traversal of the tree
  def in_order(&block)
    @left.each(&block) unless @left.nil?
    block.call(@value) if @value and block
    @right.each(&block) unless @right.nil?
  end
  alias :each :in_order

  # Does an pre-order traversal of the tree
  def pre_order(&block)
    block.call(@value) if @value and block
    @left.each(&block) unless @left.nil?
    @right.each(&block) unless @right.nil?
  end

  # Does an post-order traversal of the tree
  def post_order(&block)
    @left.each(&block) unless @left.nil?
    @right.each(&block) unless @right.nil?
    block.call(@value) if @value and block
  end

  def nearest(target_point, depth=0)
    nearest_leaf = target_point[@axis]
  end

  def nearest_leaf_path(target_point, path)
    path << self

    nearest_leaf_path(target_point, path)
    path
  end

  def eql?(other)
    @data.eql?(other.data) and
    @left.eql?(other.left) and
    @right.eql?(other.y)
  end
end
