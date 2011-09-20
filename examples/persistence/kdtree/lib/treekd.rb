# Start of a KD Tree library.  
#
# This KD-tree is specific to Euclidean space.  This makes it difficult to
# apply to longitude/latitude problems (spherical coordinates), although
# it can produce approximate results.
# 
# This class is optimized for 1D, 2D, and 3D cases (PointKD accessor methods for
# x/y/z are used over direct indexing when possible; this yields
# a significant performance benefit).
#
# == TODO
# * It'd be nice if we could represent spherical space.  This involves
#   giving the user the ability to pass in their own hyperplane-hypersphere
#   intersection and axis-distance-check test functions (e.g. the 
#   great-circle distance formula could replace the euclidean one used 
#   for the axis distance check).  However, you must also reconcile the 
#   issue where longitude/latitude wraps around, which fundamentally
#   alters the way the algorithm needs to do searches/inserts
#   (not entirely sure this can be done just as efficiently).  There
#   are mentions online of doing a spherical->3D mapping, but this
#   gets somewhat complicated and it doesn't seem to have been 
#   implemented publicly.  As it is, you might be better off 
#   considering other data structures for latitude/longitude as suggested 
#   in the comments at 
#   http://maglevity.wordpress.com/2009/12/17/kd-trees-and-maglev/
# 
# * Perhaps add remove_point method (would behave like #remove!, but would
#   need to handle multiple equal points throughout the tree while keeping
#   in mind that PointKD equality includes the @data for that point).  Admittedly
#   not entirely useful, since real-world scenarios can rely on nearest_k
#   or are likely to already have a node reference.

require 'heap'

module Collections
  # TreeKD is a KD-Tree of dimension K.  Points in the tree exist in
  # Euclidean space and distance calculations are performed as such.
  #
  # Values are stored in both the leaves and the interior nodes.
  class TreeKD
    include Enumerable

    attr_reader :left, :right, :value, :dimension

    # Creates a new +TreeKD+ for the given points that lie in Euclidean space.
    # Points should derive from class #PointKD (extend from it).
    # If +euclidean_points+ is nil or empty, will return an empty tree.  
    # +#left+ and +#right+ may return nil, if there is no data on that side 
    # of the tree.  
    def initialize(euclidean_points, dimension, depth=0)
      @dimension = dimension
      @axis = depth % @dimension # Cycle through axis as we descend down the tree
      return if euclidean_points.nil? or euclidean_points.empty?

      # Sort and split tree by median point
      # NOTE: There is a big performance boost if use the x and y
      # accessors, rather than the [@axis] approach of:
      #    sorted = points.sort {|a,b| a[@axis] <=> b[@axis] }
      if @axis == 0
        sorted = euclidean_points.sort {|a,b| a.x <=> b.x }
      elsif @axis == 1
        sorted = euclidean_points.sort {|a,b| a.y <=> b.y }
      elsif @axis == 2
        sorted = euclidean_points.sort {|a,b| a.z <=> b.z }
      else
        sorted = euclidean_points.sort {|a,b| a[@axis] <=> b[@axis] }
      end

      pivot = sorted.size / 2
      left_points = sorted[0...pivot]
      right_points = sorted[pivot+1..-1]
      @left = TreeKD.new(left_points, @dimension, depth+1) unless left_points.nil? or left_points.empty?
      @right = TreeKD.new(right_points, @dimension, depth+1) unless right_points.nil? or right_points.empty?
      @value = sorted[pivot]
    end

    # Returns maximum node depth of tree. 
    # An empty tree has a depth of 0.  A tree with just one point has a depth of 1,
    # a balanced tree with three points has a depth of 2, and so on.
    # This is expensive; it explores the entire tree.
    def max_depth
      if @value.nil? then
        0 # Empty tree
      elsif leaf? then
        1 # Non-nil @value and is leaf
      else 
        # We already know this is not a leaf node; find the max depth
        # of children.
        left_depth = @left.nil? ? 0 : @left.max_depth
        right_depth = @right.nil? ? 0 : @right.max_depth
        if left_depth > right_depth
          left_depth + 1
        else
          right_depth + 1
        end
      end
    end

    # Determines if this tree is balanced or not. 
    # Expensive; explores the entire tree. 
    def balanced?
      if @value.nil? or leaf? then
        true # An empty tree/leaf node is already balanced
      else
        left_depth = @left.nil? ? 0 : @left.max_depth
        right_depth = @right.nil? ? 0 : @right.max_depth
        # The left/right max depths cannot differ by more than one if this tree is balanced
        (left_depth - right_depth).abs <= 1
      end
    end

    # Rebuilds the entire tree, balancing it in the process.
    # Returns the new TreeKD.
    def rebuild
      points = []
      each { |pt| points << pt }
      TreeKD.new(points, @dimension)      
    end

    # Behaves as #rebuild indicates, but allows the exclusion of points
    # in the rebuilt tree using the +exclude+ array.  Also allows
    # exclusion of just the root node through +exclude_root+.
    def _rebuild(exclude, exclude_root=false)
      exclude = [] if exclude.nil?
      points = []
      points << @value unless exclude_root or exclude.include?(@value)
      @left.each { |pt| points << pt unless exclude.include?(pt) } unless @left.nil?
      @right.each { |pt| points << pt unless exclude.include?(pt) } unless @right.nil?
      TreeKD.new(points, @dimension)
    end
    protected :_rebuild

    # Removes just the root node of this tree (that is, the "current value").  
    # After calling this method, the root value will change to one of the 
    # descendant nodes.
    # This is expensive; it will rebuild the entire tree rooted at this node.
    def remove!
      # Rebuild this tree without the root node.  
      tree = _rebuild(nil, true)

      # Replace ourself with the new tree
      @value = tree.value
      @left = tree.left
      @right = tree.right
    end

    # Inserts +point+ (a PointKD) into the tree.
    # Be aware that as you insert points, the tree may become increasingly
    # imbalanced.
    def insert_point(point)
      if @value.nil? then
        # Empty tree
        @value = point
        return
      end

      # Decide which side of the hyperplane this point goes on 
      # (for points lying on the hyperplane, the side is arbitrary).
      # After that, try to put the point there, or if it's already
      # occupied by another tree, try to put it into that tree.
      # TODO: You could use the first-three-dimensions compare here to 
      # potentially speed things up
      if point[@axis] >= @value[@axis] then
        if @right.nil? then
          @right = TreeKD.new([point], @dimension, @axis + 1)
        else
          @right.insert_point(point) 
        end
      else
        if @left.nil? then
          @left = TreeKD.new([point], @dimension, @axis + 1)
        else
          @left.insert_point(point)
        end
      end
    end

    # Prints the tree out horizontally as if the tree were rotated
    # 90 degrees counter-clockwise (left of tree is at bottom, top of tree
    # is where text begins).
    def print_tree(offset=0)      
      @right.print_tree(offset + 2) unless @right.nil?
      puts((" " * offset) + @value.to_s)      
      @left.print_tree(offset + 2) unless @left.nil?
    end

    # Does an in-order (sorted) traversal of the tree, but yields only the
    # values
    def each(&block)
      @left.each(&block) unless @left.nil?
      block.call(@value) if @value and block_given?
      @right.each(&block) unless @right.nil?
    end

    # Does an pre-order traversal of the tree, yields the whole node
    def pre_order(&block)
      block.call(self) if block_given?
      @left.pre_order(&block) unless @left.nil?
      @right.pre_order(&block) unless @right.nil?
    end

    # Does an post-order traversal of the tree, yields the whole node
    def post_order(&block)
      @left.post_order(&block) unless @left.nil?
      @right.post_order(&block) unless @right.nil?
      block.call(self) if block_given?
    end

    # Does an in-order traversal of the tree, yields the whole node
    def in_order(&block)
      @left.in_order(&block) unless @left.nil?
      block.call(self) if block_given?
      @right.in_order(&block) unless @right.nil?
    end

    def leaf?
      @left.nil? and @right.nil?
    end

    # Finds the nearest point in receiver to target_point, or nil, if there
    # are no points in receiver.
    def nearest(target_point)
      nearest_k(target_point, 1)[0] 
    end

    # Find the nearest +k+ points in receiver to +target_point+.  Returns an
    # array of at most k SearchResults. If this tree contains several
    # points at the same distance, at most k of them will be returned. 
    def nearest_k(target_point, k=1)
      bestk = BestK.new(k)
      _nearest_k(target_point, bestk)
      bestk.values
    end

    # Does not return a value, only modifies +bestk+
    def _nearest_k(target_point, bestk)
      my_result = SearchResult.new(@value, target_point.distance_sq(@value))
      if self.leaf?
        bestk.add(my_result)
        return
      end

      cmp = if @axis == 0
              target_point.x <=> @value.x
            elsif @axis == 1
              target_point.y <=> @value.y
            elsif @axis == 2
              target_point.z <=> @value.z
            else
              target_point[@axis] <=> @value[@axis]
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
      # so, if we add ourself before we call axis_too_far_from,
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
                         elsif @axis == 1
                           @value.y - target.y
                         elsif @axis == 2
                           @value.z - target.z
                         else
                            @value[@axis] - target[@axis]
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
      "Tree#{@dimension}D (inspect): value #{value.inspect} LEFT #{@left.to_s} RIGHT: #{@right.to_s}"
    end

    def to_s
      "Tree#{@dimension}D (to_s): value #{value.inspect} LEFT #{@left.__id__} RIGHT: #{@right.__id__}"
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

  # A PointKD contains K coordinates and may have some data
  # associated with it.  It's assumed that the point lies
  # in Euclidean space.
  # This class is optimized for 1D, 2D, and 3D cases; it
  # stores the first three coordinates in x, y, z fields.
  class PointKD
    attr_reader :tuple, :data, :x, :y, :z

    # Initializes this point with a +tuple+ (array) of coordinates
    def initialize(tuple, data=nil)
      @tuple, @data = tuple, data
      @x = tuple[0]
      @y = tuple[1]
      @z = tuple[2] 
    end

    def to_s
      "Point[#{@tuple.join(', ')}] #{@data}"
    end

    # Returns the coordinate along the given axis (starting at 0).
    # For 1D, 2D, and 3D points, it is better to use the x/y/z accessors.
    def [](index)
      if index < 0 or index >= @tuple.size then
        raise ArgumentError, "#{index} is out of range. Should be between 0 and #{@tuple.size-1}"
      end      
      @tuple[index]
    end

    def ==(other)
      eql?(other)
    end

    def eql?(other)
      @tuple.eql?(other.tuple) and @data.eql?(other.data)
    end

    def hash
      @tuple.hash ^ @data.hash
    end

    # Determines the distance-squared to +other+ via the Euclidean distance formula.
    def distance_sq(other)
      # This casy-by-case approach does have a noticable performance impact, 
      # at least in the 2D case
      case @tuple.size
      when 1
        dx = @x - other.x
        dx * dx
      when 2
        dx = @x - other.x
        dy = @y - other.y
        (dx * dx) + (dy * dy)
      when 3
        dx = @x - other.x
        dy = @y - other.y
        dz = @z - other.z
        (dx * dx) + (dy * dy) + (dz * dz)
      else
        sum = 0
        @tuple.each_index { |i|
          sum += (other.tuple[i] - @tuple[i])**2 
        }
        sum
      end
    end
  end
end
