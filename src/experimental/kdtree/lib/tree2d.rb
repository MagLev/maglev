# Tree2D is a KD-Tree of dimension 2.
#
# The points given to a tree must have the following methods:
#   cmp(axis, other)
#
#
# TODO: Instead of checking axis all the time, make subclasses for each axis?
class Tree2D
  attr_reader :left, :right, :value

  # Creates a new Tree2D for the given points.  If points is nil or empty,
  # will return an empty tree.  The @left and @right instance variables may
  # be null, if there is no data on that side of the tree.
  #
  # TODO: Should we replace nil with an empty tree?
  def initialize(points, depth=0)
    @axis = depth % 2
    return if points.nil? or points.empty?

    sorted = points.sort {|a,b| a[@axis] <=> b[@axis] }
    pivot = sorted.size / 2
    left_points = sorted[0...pivot]
    right_points = sorted[pivot+1..-1]
    @left = Tree2D.new(left_points, depth+1) unless left_points.nil? or left_points.empty?
    @right = Tree2D.new(right_points, depth+1) unless right_points.nil? or right_points.empty?
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

  def leaf?
    @left.nil? and @right.nil?
  end

  # Returns an array of [value, dist] which represents the value of the
  # nearest point to the target_point in this subtree, along with the
  # distance squared from the point to the target_point. If this tree
  # contains several points at the same distance, only one of those values
  # is returned.
  def nearest(target_point)
    my_dist = target_point.dist_sq(@value)
    best_v = @value
    best_d = my_dist

    return [best_v, best_d] if self.leaf?

    cmp = target_point[@axis] <=> @value[@axis]
    # But best_v is not set if the search side is nil...
    case cmp
    when -1
      unsearched = @right
      best_v, best_d = @left.nearest(target_point) unless @left.nil?
    when 1
      unsearched = @left
      best_v, best_d = @right.nearest(target_point) unless @right.nil?
    when 0
      if @left
        unsearched = @right
        @left.nearest(target_point)
      else
        unsearched = @left
        @right.nearest(target_point)
      end
    end

    # Am I better than the best in my sub-tree?
    if my_dist < best_d
      best_v = @value
      best_d = my_dist
    end

    # Check if the other side of the splitting plane is close enough for
    # possibilities.  This will be the case if the best distance so far is
    # larger than the distance from target node to the axis (i.e., does a
    # hypersphere of radius best_d cross the splitting axis or not).
    target_to_axis_d = @value[@axis] - best_v[@axis]
    target_to_axis_d_sq = target_to_axis_d * target_to_axis_d
    if best_d >= target_to_axis_d and not unsearched.nil?
      v, d = unsearched.nearest(target_point)
      if d < best_d
        best_v = v
        best_d = d
      end
    end
    [best_v, best_d]
  end

  def eql?(other)
    @value.eql?(other.value) and
    @left.eql?(other.left) and
    @right.eql?(other.y)
  end

  def inspect
    "Tree2D: value #{value.inspect} LEFT #{@left.to_s} RIGHT: #{@right.to_s}"
  end
end
