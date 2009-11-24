# Start of a KD Tree library.  Currently, only 2D trees are supported, but
# the general algorithm should work for higher dimensions.  There are a few
# optimizations for 2D trees (basically, instead of iterating through the
# axis by using Point[], we access with x() and y(), which cuts down the
# running time by quite a bit.
#
# == TODO
# * Add insert_node and remove_node methods (make them (functionally)
#   persistent).
#
# * Rebalance is hard for KD Trees.  Need to implement a re-generate
#   which just completely rebuilds the tree

require 'heap'

module Collections
  # Tree2D is a KD-Tree of dimension 2.
  #
  # Values are stored in both the leaves and the interior nodes.
  class Tree2D
    include Enumerable

    # Create a Tree2D with +size+ nodes randomly located.
    def self.random(size=1_000)
      new(Array.new(size) { |i| Point2D.random("point #{i}") })
    end

    attr_reader :left, :right, :value

    # Creates a new +Tree2D+ for the given points.  If +points+ is nil or
    # empty, will return an empty tree.  +#left+ and +#right+ may return
    # nil, if there is no data on that side of the tree.  The points passed
    # to the initialize method must respond to the following methods:
    # * #x Return the x coordinate
    # * #y Return the y coordinate
    # * #eql?  (and hash)
    # * #distance(other) Return the distance from the other point
    #
    def initialize(points, depth=0)
      @axis = depth % 2
      return if points.nil? or points.empty?

      # NOTE: There is a big performance boost if use the x and y
      # accessors, rather than the [@axis] approach of:
      #    sorted = points.sort {|a,b| a[@axis] <=> b[@axis] }
      if @axis == 0
        sorted = points.sort {|a,b| a.x <=> b.x }
      else
        sorted = points.sort {|a,b| a.y <=> b.y }
      end
      pivot = sorted.size / 2
      left_points = sorted[0...pivot]
      right_points = sorted[pivot+1..-1]
      @left = Tree2D.new(left_points, depth+1) unless left_points.nil? or left_points.empty?
      @right = Tree2D.new(right_points, depth+1) unless right_points.nil? or right_points.empty?
      @value = sorted[pivot]
    end

    # Does an in-order (sorted) traversal of the tree, but yields only the
    # values
    def each(&block)
      @left.each(&block) unless @left.nil?
      block.call(@value) if @value and block
      @right.each(&block) unless @right.nil?
    end

    # Does an pre-order traversal of the tree, yields the whole node
    def pre_order(&block)
      block.call(self) if block
      @left.pre_order(&block) unless @left.nil?
      @right.pre_order(&block) unless @right.nil?
    end

    # Does an post-order traversal of the tree, yields the whole node
    def post_order(&block)
      @left.post_order(&block) unless @left.nil?
      @right.post_order(&block) unless @right.nil?
      block.call(self) if block
    end

    # Does an in-order traversal of the tree, yields the whole node
    def in_order(&block)
      @left.in_order(&block) unless @left.nil?
      block.call(self) if block
      @right.in_order(&block) unless @right.nil?
    end

    def leaf?
      @left.nil? and @right.nil?
    end

    def nearest_lat_lon(lat, lon)
      nearest(Point2D.new(lat, lon))
    end

    # Finds the nearest point in receiver to target_point, or nil, if there
    # are no points in receiver.
    def nearest(target_point)
      nearest_k(target_point, 1)[0]
    end

    # Find the nearest +k+ nodes in receiver to +target_point+.  Returns an
    # array of at most k SearchResults. If this tree contains several
    # points at the same distance, at most k of them will be returned.
    def nearest_k(target_point, k=1)
      bestk = BestK.new(k)
      _nearest_k(target_point, bestk)
      bestk.values
    end

    # Does not return a value, only modifies +bestk+
    def _nearest_k(target_point, bestk)
      my_result = SearchResult.new(@value, target_point.distance(@value))
      if self.leaf?
        bestk.add(my_result)
        return
      end

      cmp = if @axis == 0
              target_point.x <=> @value.x
            else
              target_point.y <=> @value.y
            end
      case cmp
      when -1
        unsearched = @right
        @left._nearest_k(target_point, bestk) unless @left.nil?
      when 1
        unsearched = @left
        @right._nearest_k(target_point, bestk) unless @right.nil?
      when 0
        if @left
          unsearched = @right
          @left._nearest_k(target_point, bestk)
        else
          unsearched = @left
          @right._nearest_k(target_point, bestk)
        end
      end

      # We do not need to search the other child if
      # A: we don't have another child OR
      # B: we (a) already have enough candidates (bestk is full) and (b) we
      #    are too far from the axis.
      unless unsearched.nil? or (bestk.full? and axis_too_far_from(target_point, bestk))
        unsearched._nearest_k(target_point, bestk)
      end

      # Add ourself only after we check whether to search the unsearched
      # tree.  The reason is that:
      #    our_distance_to_target >= target_distance_to_axis
      # so, if we add ourself before we call split_plane_close_enough_to,
      # then we will be in bestk, so bestk distance can't be smaller than
      # our distance, hence we will always search the other side (correct
      # results, but inefficient).
      bestk.add(my_result)
    end
    protected :_nearest_k

    # Do we need to search the other side of our axis?  Or is the target
    # node too far from the axis that we know there can't be anything
    # closer?  We need to check the other side if the best distance so far
    # is larger than the distance from target node to my splitting axis
    # (i.e., does a hypersphere of radius bestk cross the splitting axis or
    # not).
    def axis_too_far_from(target, bestk)
      target_to_axis_d = if @axis == 0
                           @value.x - target.x
                         else
                           @value.y - target.y
                         end
      target_to_axis_d_sq = target_to_axis_d * target_to_axis_d
      bestk.worst.distance < target_to_axis_d_sq
    end

    def eql?(other)
      @value.eql?(other.value) and
        @left.eql?(other.left) and
        @right.eql?(other.y)
    end

    def inspect
      "Tree2D (inspect): value #{value.inspect} LEFT #{@left.to_s} RIGHT: #{@right.to_s}"
    end

    def to_s
      "Tree2D (to_s): value #{value.inspect} LEFT #{@left.__id__} RIGHT: #{@right.__id__}"
    end
  end

  # A small class to hold a value and its distance from some other point.
  # It implements > so that it can be used by the standard comparator in
  # nearest.
  class SearchResult
    include Comparable
    attr_reader :value, :distance
    def initialize(value, dist)
      @value = value
      @distance = dist
    end

    def <=>(other)
      other.distance <=> @distance
    end
  end

  # A Point2D contains two coordinates, x and y, and may have some data
  # associated with it.
  class Point2D
    MAX_SCALAR = 360.0
    MID_POINT  = MAX_SCALAR / 2.0

    # Return Point2D with random lat and lon.  Default name is :target.
    def self.random(name=:target)
      new(rand(MAX_SCALAR) - MID_POINT, rand(MAX_SCALAR) - MID_POINT, name)
    end

    attr_reader :x, :y, :data

    def initialize(x, y, data=nil)
      @x, @y, @data = x, y, data
    end

    def to_s
      "Point[#{@x}, #{@y}] #{@data}"
    end

    # Return the coordinate along the given axis (0 or 1)
    # This is much slower than using the x() and y() readers...
    def [](index)
      if index.equal? 0
        @x
      elsif index.equal? 1
        @y
      else
        raise ArgumentError, "#{index} is out of range.  Should be 0 or 1"
      end
    end

    def eql?(other)
      @x.eql?(other.x) and @y.eql?(other.y) and @data.eql?(other.data)
    end

    def hash
      @x.hash ^ @y.hash ^ @data.hash
    end

    # Returns the distance squared from the other point
    def distance(other)
      dx = @x - other.x
      dy = @y - other.y
      (dx * dx) + (dy * dy)
    end
  end
end
