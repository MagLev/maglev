require 'rubygems'
require 'minitest/spec'
require 'tree2d'

MiniTest::Unit.autorun

describe KDTree::Tree2D do
  before do
    @empty_tree = KDTree::Tree2D.new []
    @p1 = KDTree::Point2D.new(0, 1, :point1)
    @p2 = KDTree::Point2D.new(0, 2, :point2)
    @one_pt_tree = KDTree::Tree2D.new [@p1]
    @two_pt_tree = KDTree::Tree2D.new [@p1, @p2]
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
      tree = KDTree::Tree2D.new [@p1, @p1, @p1, @p1, @p1]
      tree.wont_be_nil
      tree.left.wont_be_nil
      tree.right.wont_be_nil
      tree.value.must_equal @p1
      count = 0
      tree.each {|el| count += 1}
      count.must_equal 5
    end

    it 'splits first on axis 0' do
      a = KDTree::Point2D.new(-1,  0, :a)
      b = KDTree::Point2D.new( 0,  0, :b)
      c = KDTree::Point2D.new( 1,  0, :c)
      t1 = KDTree::Tree2D.new [a, b, c]
      t2 = KDTree::Tree2D.new [c, a, b]
      t3 = KDTree::Tree2D.new [b, a, c]
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
      a = KDTree::Point2D.new( 0, -2, :a)
      b = KDTree::Point2D.new(-1,  1, :b)
      c = KDTree::Point2D.new( 1,  0, :c)
      d = KDTree::Point2D.new(-2, -1, :d)
      e = KDTree::Point2D.new(-3,  2, :e)
      f = KDTree::Point2D.new( 2, -1, :f)
      g = KDTree::Point2D.new( 3,  2, :g)
      tree = KDTree::Tree2D.new [a, b, c, d, e, f, g]
      in_order_traversal = [d, b, e, a, f, c, g]
      tree.each { |el| el.must_equal in_order_traversal.shift }
    end
  end

  describe 'finding and traversing fixed graphs' do
    before do
      @tree = KDTree::Tree2D.new(
        [@a = KDTree::Point2D.new( 0, -2, :a),
         @b = KDTree::Point2D.new(-1,  1, :b),
         @c = KDTree::Point2D.new( 1,  0, :c),
         @d = KDTree::Point2D.new(-2, -1, :d),
         @e = KDTree::Point2D.new(-3,  2, :e),
         @f = KDTree::Point2D.new( 2, -1, :f),
         @g = KDTree::Point2D.new( 3,  2, :g)])
    end

    it 'finds the nearest point in known graph' do
      target = KDTree::Point2D.new(-2, 1, :target_point)
      v, d = @tree.nearest(target)
      v.must_equal @b
    end

    it 'finds the nearest k points in known graph' do
      target = KDTree::Point2D.new(-2, 1, :target_point)
      k = 1
      best = @tree.nearest_k(target, k)
      best.value.must_equal @b
    end
  end

  describe 'finding and iterating random graphs' do
    # Random data will be constrained by:
    NUM_DIMENSIONS = 2
    MAX_SCALAR  = 100
    MAX_DIST_SQ = MAX_SCALAR * MAX_SCALAR * NUM_DIMENSIONS

    before do
      @points = Array.new
      100.times {|i| @points << KDTree::Point2D.new(rand(MAX_SCALAR), rand(MAX_SCALAR), "point #{i}")}
      @tree = KDTree::Tree2D.new @points
    end

    it 'traverses all nodes with each' do
      seen = Hash.new(0)
      @tree.each {|el| seen[el.data] += 1 }
      seen.size.must_equal @points.size
    end

    it 'finds the nearest point (random float data)' do
      100.times do |i|
        target = KDTree::Point2D.new(50.01 - rand(MAX_SCALAR),
                                     49.87 - rand(MAX_SCALAR),
                                     :target_point)
        expected_p, expected_d = find_nearest(target, @points)
        expected_p.wont_be_nil
        expected_d.wont_be_nil
        actual_p, actual_d = @tree.nearest(target)
      end
    end

    it 'finds the same nodes with nearest and nearest_k' do
      500.times do |i|
        target = KDTree::Point2D.new(50.01 - rand(MAX_SCALAR),
                                     49.87 - rand(MAX_SCALAR),
                                     :target_point)
        expected_p, expected_d = find_nearest(target, @points)
        expected = find_nearest_k(target, @points, 1)

        actual_p, actual_d = @tree.nearest(target)
        actual = @tree.nearest_k(target, 1)

        # With random data, we may have two nodes with the same
        # coordinates, or two points that are equidistant from the target.
        # Do not compare points, only the distances.
        expected_d.must_equal expected.metric

        actual.metric.must_equal expected.metric
        actual_d.must_equal expected_d
      end
    end

    it 'finds the nearest_k points (random float data)' do
      500.times do |i|
        target = KDTree::Point2D.new(50.01 - rand(MAX_SCALAR),
                                     49.87 - rand(MAX_SCALAR),
                                     :target_point)
        expected = find_nearest_k(target, @points, 1)
        expected.wont_be_nil
        expected.value.wont_be_nil
        expected.metric.wont_be_nil

        actual = @tree.nearest_k(target, 1)

        # With random data, we may have two nodes with the same
        # coordinates, or two points that are equidistant from the target.
        # Do not compare points, only the distances.
        actual.metric.must_equal expected.metric
      end
    end

    it 'finds nearest_k with an exact match high up the tree' do
    end

    it 'finds nearest_k with k > num nodes' do
    end

    it 'finds nearest_k in degenerate trees' do
    end

    # Do an exhaustive search of the random data for the nearest point.
    # Used to find the expected value for the random trees.  Returns an
    # array of the nearest point and the distance from the target to that
    # point.
    def find_nearest(target, points)
      points.inject([nil, MAX_DIST_SQ]) do |acc, current|
        best_d = acc[1]
        cur_dist = current.dist_sq(target)
        cur_dist < best_d ? [current, cur_dist] : acc
      end
    end

    def find_nearest_k(target, points, k=1)
      best = KDTree::MinMetric.new
      points.inject(best) do |acc, current|
        best.update(current, current.dist_sq(target))
        best
      end
    end
  end
end

