require 'rubygems'
require 'minitest/spec'
require 'tree2d'
require 'heap'

MiniTest::Unit.autorun

describe Collections::Tree2D do
  before do
    @empty_tree = Collections::Tree2D.new []
    @p1 = Collections::Point2D.new(0, 1, :point1)
    @p2 = Collections::Point2D.new(0, 2, :point2)
    @one_pt_tree = Collections::Tree2D.new [@p1]
    @two_pt_tree = Collections::Tree2D.new [@p1, @p2]

    # Prepare several example trees that have some properties we will test later
    @dp1 = Collections::Point2D.new(1, 1, :depthpoint1)
    @dp2 = Collections::Point2D.new(2, 2, :depthpoint2)
    @dp3 = Collections::Point2D.new(3, 3, :depthpoint3)
    @dp4 = Collections::Point2D.new(-1, -1, :depthpoint4)
    @dp5 = Collections::Point2D.new(-2, -2, :depthpoint5)
    @depth0_tree = Collections::Tree2D.new [] # balanced
    @depth1_tree = Collections::Tree2D.new [@dp1] # balanced
    @depth2_tree_a = Collections::Tree2D.new [@dp1, @dp2] # balanced
    @depth2_tree_b = Collections::Tree2D.new [@dp1, @dp2, @dp3] # balanced

    @depth3_tree_a = Collections::Tree2D.new [@dp1, @dp2] # unbalanced (note: @dp2 will be root)
    @depth3_tree_a.insert_point @dp4

    @depth3_tree_b = Collections::Tree2D.new [@dp1] # unbalanced
    @depth3_tree_b.insert_point @dp4 # Falls to left
    @depth3_tree_b.insert_point Collections::Point2D.new(0, 0, :zeropoint) # Falls to right of @dp4

    @depth3_tree_c = Collections::Tree2D.new(((1..6).to_a.map { |n| # balanced
       Collections::Point2D.new(n, n)
    }))
  end

  describe 'for tree node removal' do
    it 'removes from a 1-point tree' do
      @one_pt_tree.remove!
      @one_pt_tree.value.must_be_nil
      @one_pt_tree.right.must_be_nil
      @one_pt_tree.left.must_be_nil
    end

    it 'removes from a 2-point tree' do
      @two_pt_tree.remove!
      @two_pt_tree.value.wont_be_nil
      @two_pt_tree.right.must_be_nil
      @two_pt_tree.left.must_be_nil
    end

    it 'removes from a 3-point tree' do
      @three_pt_tree = Collections::Tree2D.new [@dp1, @dp2, @dp3]
      @three_pt_tree.remove!
      @three_pt_tree.value.must_equal @dp3
      @three_pt_tree.right.must_be_nil
      @three_pt_tree.left.value.must_equal @dp1      
    end

    it 'removes an interior node from a depth 3 tree' do
      # Create an unbalanced tree that extends to the right
      # and then remove an interior node.
      @top_pt      = Collections::Point2D.new(0, 0, :top)
      @interior_pt = Collections::Point2D.new(1, 0, :interior)
      @leaf_pt     = Collections::Point2D.new(1, 1, :leaf)
      @tree = Collections::Tree2D.new [@top_pt]
      @tree.insert_point(@interior_pt)
      @tree.insert_point(@leaf_pt)

      # Verify tree is set up right
      @tree.value.must_equal @top_pt
      @tree.right.value.must_equal @interior_pt 
      @tree.right.right.value.must_equal @leaf_pt
   
      @tree.right.remove! # Remove interior

      # Verify modified tree is correct
      @tree.value.must_equal @top_pt
      @tree.right.value.must_equal @leaf_pt
      @tree.right.left.must_be_nil
      @tree.right.right.must_be_nil
    end
  end

  describe 'balanced tree' do
    it 'determines if trees are balanced' do
      @depth0_tree.balanced?.must_equal true 
      @depth1_tree.balanced?.must_equal true 
      @depth2_tree_a.balanced?.must_equal true 
      @depth2_tree_b.balanced?.must_equal true 
      @depth3_tree_a.balanced?.must_equal false
      @depth3_tree_b.balanced?.must_equal false
      @depth3_tree_c.balanced?.must_equal true
    end
  end

  describe 'for maximum depth' do
    it 'finds correct depths' do
      @depth0_tree.max_depth.must_equal 0 
      @depth1_tree.max_depth.must_equal 1
      @depth2_tree_a.max_depth.must_equal 2
      @depth2_tree_b.max_depth.must_equal 2
      @depth3_tree_a.max_depth.must_equal 3
      @depth3_tree_b.max_depth.must_equal 3
      @depth3_tree_c.max_depth.must_equal 3
    end
  end

  describe 'for insertion' do
    before do  
      @insert_tree = Collections::Tree2D.new []
    end
 
    it 'inserts into an empty tree to establish root' do      
      @insert_tree.insert_point(@p1)
      @insert_tree.value.must_equal @p1
      @insert_tree.right.must_be_nil
      @insert_tree.left.must_be_nil
    end

    it 'inserts into balanced 1-node tree (insertion value same as root)' do 
      @insert_tree.insert_point(@dp1)
      @insert_tree.insert_point(@dp1)
      @insert_tree.value.must_equal @dp1
      if @insert_tree.right.value then # "side" of hyperplane it ends up on is arbitrary
        @insert_tree.right.value.must_equal @dp1 
        @insert_tree.right.left.must_be_nil
        @insert_tree.right.right.must_be_nil      
        @insert_tree.left.must_be_nil
      else
        @insert_tree.left.value.must_equal @dp1 
        @insert_tree.left.left.must_be_nil
        @insert_tree.left.right.must_be_nil      
        @insert_tree.right.must_be_nil 
      end
    end

    it 'inserts into balanced 1-node tree (insert right)' do 
      @insert_tree.insert_point(@dp1)
      @insert_tree.insert_point(@dp2)
      @insert_tree.value.must_equal @dp1
      @insert_tree.right.value.must_equal @dp2
      @insert_tree.right.left.must_be_nil
      @insert_tree.right.right.must_be_nil
      @insert_tree.left.must_be_nil
    end

    it 'inserts into balanced 1-node tree (insert left)' do       
      @insert_tree.insert_point(@dp2)
      @insert_tree.insert_point(@dp1)
      @insert_tree.value.must_equal @dp2
      @insert_tree.right.must_be_nil
      @insert_tree.left.value.must_equal @dp1
      @insert_tree.left.left.must_be_nil
      @insert_tree.left.right.must_be_nil
    end
  end

  describe 'creation' do
    it 'creates an empty tree' do
      @empty_tree.wont_be_nil
      @empty_tree.left.must_be_nil
      @empty_tree.right.must_be_nil
      @empty_tree.value.must_be_nil
    end

    it 'creates a tree with one point' do
      @one_pt_tree.wont_be_nil
      @one_pt_tree.left.must_be_nil
      @one_pt_tree.right.must_be_nil
      @one_pt_tree.value.must_equal @p1
    end

    it 'creates a tree with two points' do
      @two_pt_tree.wont_be_nil
      @two_pt_tree.left.wont_be_nil
      @two_pt_tree.right.must_be_nil
      @two_pt_tree.value.must_equal @p2
    end

    it 'creates a tree with all the same points' do
      tree = Collections::Tree2D.new [@p1, @p1, @p1, @p1, @p1]
      tree.wont_be_nil
      tree.left.wont_be_nil
      tree.right.wont_be_nil
      tree.value.must_equal @p1
      count = 0
      tree.each {|el| count += 1}
      count.must_equal 5
    end

    it 'splits first on axis 0' do
      a = Collections::Point2D.new(-1,  0, :a)
      b = Collections::Point2D.new( 0,  0, :b)
      c = Collections::Point2D.new( 1,  0, :c)
      t1 = Collections::Tree2D.new [a, b, c]
      t2 = Collections::Tree2D.new [c, a, b]
      t3 = Collections::Tree2D.new [b, a, c]
      [t1, t2, t3].each do |t|
        t.left.value.must_equal a
        t.left.left.must_be_nil
        t.left.right.must_be_nil

        t.value.must_equal b

        t.right.value.must_equal c
        t.right.left.must_be_nil
        t.right.right.must_be_nil
      end
    end

    it 'splits second on axis 1' do
      a = Collections::Point2D.new( 0, -2, :a)
      b = Collections::Point2D.new(-1,  1, :b)
      c = Collections::Point2D.new( 1,  0, :c)
      d = Collections::Point2D.new(-2, -1, :d)
      e = Collections::Point2D.new(-3,  2, :e)
      f = Collections::Point2D.new( 2, -1, :f)
      g = Collections::Point2D.new( 3,  2, :g)
      tree = Collections::Tree2D.new [a, b, c, d, e, f, g]
      in_order_traversal = [d, b, e, a, f, c, g]
      tree.each { |el| el.must_equal in_order_traversal.shift }
    end
  end

  describe 'finding and traversing fixed graphs' do
    before do
      @tree = Collections::Tree2D.new(
        [@a = Collections::Point2D.new( 0, -2, :a),
         @b = Collections::Point2D.new(-1,  1, :b),
         @c = Collections::Point2D.new( 1,  0, :c),
         @d = Collections::Point2D.new(-2, -1, :d),
         @e = Collections::Point2D.new(-3,  2, :e),
         @f = Collections::Point2D.new( 2, -1, :f),
         @g = Collections::Point2D.new( 3,  2, :g)])
    end

    it 'finds the nearest point in known graph' do
      target = Collections::Point2D.new(-2, 1, :target_point)
      v = @tree.nearest(target)
      v.value.must_equal @b
    end

    it 'finds the nearest k points in known graph' do
      target = Collections::Point2D.new(-2, 1, :target_point)

      best = @tree.nearest_k(target, 3).map {|sr| sr.value }

      best.include?(@b).must_equal true
      best.include?(@d).must_equal true
      best.include?(@e).must_equal true
    end

    it 'handles degenerate case of nearest_k' do
      # This test case sets up the situation where we will have a bestk of
      # 0, but we still need to search the other side of the splitting
      # plane, because we have not yet found k points.
      @tree = Collections::Tree2D.new(
        [@a = Collections::Point2D.new(  0.0,  0.0, :a),
         @b = Collections::Point2D.new( -1.0, -1.0, :b),
         @c = Collections::Point2D.new(  1.1,  1.1, :c)])

      best = @tree.nearest_k(@a, 1).map {|sr| sr.value}
      best.include?(@a).must_equal true

      best = @tree.nearest_k(@a, 2).map {|sr| sr.value}
      best.include?(@a).must_equal true
      best.include?(@b).must_equal true

      # The real danger for this test case, is that we don't find node c
      # (the worst), since it is a leaf on the other side of the splitting
      # plane, and we won't have a full bestk when we come time to decide
      # whether to search that side or not.
      best = @tree.nearest_k(@a, 100).map {|sr| sr.value}
      best.include?(@a).must_equal true
      best.include?(@b).must_equal true
      best.include?(@c).must_equal true
    end
  end

  describe 'finding and iterating random graphs' do
    # Random data will be constrained by:
    NUM_DIMENSIONS = 2
    MAX_SCALAR  = 100
    MAX_DIST_SQ = MAX_SCALAR * MAX_SCALAR * NUM_DIMENSIONS

    before do
      @points = Array.new(1_000) do |i|
        Collections::Point2D.new(rand(MAX_SCALAR),
                                 rand(MAX_SCALAR),
                                 "point #{i}")
      end
      @tree = Collections::Tree2D.new @points
      @target = Collections::Point2D.new(50.01 - rand(MAX_SCALAR),
                                         49.87 - rand(MAX_SCALAR),
                                         :target_point)
    end

    it 'traverses all nodes with each' do
      seen = Hash.new(0)
      @tree.each {|el| seen[el.data] += 1 }
      seen.size.must_equal @points.size
    end

    it 'finds the nearest point (random float data)' do
      100.times do |i|
        expected = find_nearest_k(@target, @points, 1)[0]
        expected.value.wont_be_nil
        expected.distance.wont_be_nil
        actual = @tree.nearest(@target)
        actual.distance.must_equal expected.distance
      end
    end

    it 'finds the same nodes with nearest and nearest_k' do
      500.times do |i|
        expected = find_nearest_k(@target, @points, 1)[0]
        expected.wont_be_nil

        actual   = @tree.nearest(@target)
        actual_k = @tree.nearest_k(@target, 1)[0]

        # With random data, we may have two nodes with the same
        # coordinates, or two points that are equidistant from the target.
        # Do not compare points, only the distances.
        expected.distance.must_equal actual.distance
        expected.distance.must_equal actual_k.distance
      end
    end

    it 'finds the nearest_k points (random float data)' do
      500.times do |i|
        k = 20
        expected = find_nearest_k(@target, @points, k)
        expected.wont_be_nil
        actual = @tree.nearest_k(@target, k)

        # With random data, we may have two nodes with the same
        # coordinates, or two points that are equidistant from the target.
        # Do not compare points, only the distances.
        actual.each do |a|
          exact_match   = expected.any?{ |e| a.value == e.value }
          same_distance = expected.any?{ |e| a.distance == e.distance }
          (exact_match or same_distance).must_equal true
        end
      end
    end

    # Exhaustive search of points for nodes near target.  Returns an array
    # of at most k SearchResult objects.  Each SearchResult has a node and
    # the distance_squared of that node from the target.
    def find_nearest_k(target, points, k=1)
      points.inject(Collections::BestK.new(k)) do |b, current|
        b.add(Collections::SearchResult.new(current, current.distance_sq(target)))
      end.values
    end
  end
end

