require 'rubygems'
require 'minitest/spec'
require 'heap'

MiniTest::Unit.autorun

describe Collections::Heap do
  it 'calculates parent, left and right indicies correctly' do
    # Array of pairs: [node, left, right]
    expected = [[0, 1, 2], [1, 3, 4], [2, 5, 6]]
    expected.each do |(parent, left, right)|
      Collections::Heap.parent_idx(left).must_equal parent
      Collections::Heap.parent_idx(right).must_equal parent
      Collections::Heap.left_idx(parent).must_equal left
      Collections::Heap.right_idx(parent).must_equal right
    end
    Collections::Heap.parent_idx(0).must_equal -1
  end

  it 'creates manages size properly' do
    h = Collections::Heap.new
    h.size.must_equal 0
    (1...10).each do |i|
      h.add(i)
      h.size.must_equal i
    end
  end

  it 'returns elements in min order by default' do
    h = Collections::Heap.new
    elements = [0,1,2,3,4,5,6,7,8,9].sort_by {|i| rand(100) }
    elements.each {|el| h.add(el) }
    10.times { |i| h.delete_top.must_equal i }
  end

  it 'returns elements in max order if so configured' do
    h = Collections::Heap.new {|a,b| a > b}
    elements = [0,1,2,3,4,5,6,7,8,9].sort_by {|i| rand(100) }
    elements.each {|el| h.add(el) }
    10.times { |i| h.delete_top.must_equal(9 - i) }
  end
end
