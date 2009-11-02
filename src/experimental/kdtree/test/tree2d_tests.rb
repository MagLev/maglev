require 'rubygems'
require 'minitest/spec'
require 'tree2d'
require 'point'

MiniTest::Unit.autorun

describe Tree2D do
  before do
    @empty_tree = Tree2D.new []
    @p1 = Point.new(0, 1, :point1)
    @p2 = Point.new(0, 2, :point2)
    @one_pt_tree = Tree2D.new [@p1]
    @two_pt_tree = Tree2D.new [@p1, @p2]
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
      @two_pt_tree.right.wont_be_nil
      @two_pt_tree.value.must_equal @p2
    end

    it 'creates a tree with all the same points' do
      tree = Tree2D.new [@p1, @p1, @p1, @p1, @p1]
      tree.wont_be_nil
      tree.left.wont_be_nil
      tree.right.wont_be_nil
      tree.value.must_equal @p1
    end

    it 'splits first on axis 0' do
      a = Point.new(-1,  0, :a)
      b = Point.new( 0,  0, :b)
      c = Point.new( 1,  0, :c)
      t1 = Tree2D.new [a, b, c]
      t2 = Tree2D.new [c, a, b]
      t3 = Tree2D.new [b, a, c]
    end

    it 'creates our special tree ok' do
      a = Point.new( 0, -2, :a)
      b = Point.new(-1,  1, :b)
      c = Point.new( 1,  0, :c)
      d = Point.new(-3,  2, :d)
      e = Point.new(-2, -1, :e)
      f = Point.new( 3,  2, :f)
      g = Point.new( 2, -1, :g)
      tree = Tree2D.new [a, b, c, d, e, f, g]
      in_order_traversal = [d, b, e, a, f, c, g]
      depth_first_order  = [d, e, b, f, g, c, a]
#      tree.each { |el| el.value.must_equal depth_first_order.shift }
#      tree.each { |el| p el }
    end
  end

  describe 'iteration' do
    before do
      @points = Array.new
      30.times {|i| @points << Point.new(rand(100), rand(100), "point #{i}")}
      @tree = Tree2D.new @points
    end

    it 'traverses all nodes with each' do
      seen = Hash.new(0)
      @tree.each {|el| seen[el.data] += 1 }
      seen.size.must_equal @points.size
    end
  end
end
