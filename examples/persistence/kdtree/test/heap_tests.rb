require 'rubygems'
require 'minitest/spec'
require 'heap'

MiniTest::Unit.autorun

describe Heap do
  it 'calculates parent, left and right indicies correctly' do
    # Array of pairs: [node, left, right]
    expected = [[0, 1, 2], [1, 3, 4], [2, 5, 6]]
    expected.each do |(parent, left, right)|
      Heap::Heap.parent_idx(left).must_equal parent
      Heap::Heap.parent_idx(right).must_equal parent
      Heap::Heap.left_idx(parent).must_equal left
      Heap::Heap.right_idx(parent).must_equal right
    end
    Heap::Heap.parent_idx(0).must_equal -1
  end

  it 'creates manages size properly' do
    h = Heap::Heap.new
    h.size.must_equal 0
    (1...10).each do |i|
      h.add(i)
      h.size.must_equal i
    end
  end

  it 'returns elements in min order by default' do
    h = Heap::Heap.new
    elements = [0,1,2,3,4,5,6,7,8,9].sort_by {|i| rand(100) }
    elements.each {|el| h.add(el) }
    10.times { |i| h.delete_top.must_equal i }
  end

  it 'returns elements in max order if so configured' do
    h = Heap::Heap.new {|a,b| a > b}
    elements = [0,1,2,3,4,5,6,7,8,9].sort_by {|i| rand(100) }
    elements.each {|el| h.add(el) }
    10.times { |i| h.delete_top.must_equal(9 - i) }
  end
end

describe Heap::NHeap do
  it 'does not grow beyond N elements' do
    n = 20
    h = Heap::NHeap.new(n)
    h.size.must_equal 0

    input_size = n * 20
    input_elements = Array.new(input_size) {|i| i}.sort_by{ |i| rand }
    input_elements.each do |el|
      h.add(el)
      (n >= h.size).must_equal true
    end

    h.size.must_equal n

    n.times { |i| h.delete_top.must_equal(input_size - n + i) }
  end

  it 'maintains smallest N if cmp is >' do
    n = 20
    h = Heap::NHeap.new(n) {|a,b| a > b}
    h.size.must_equal 0

    input_size = n * 20
    input_elements = Array.new(input_size) {|i| i}.sort_by{ |i| rand }
    input_elements.each do |el|
      h.add(el)
      (n >= h.size).must_equal true
    end

    h.size.must_equal n

    n.times { |i| h.delete_top.must_equal(n-1-i) }
  end

  it 'knows when it is full' do
    n = 20
    h = Heap::NHeap.new(n) {|a,b| a > b}
    h.full?.must_equal false
    19.times do |i|
      h.add(i)
      h.full?.must_equal false
    end

    # We have N-1 elements, so we should become full, and remain full
    5.times do
      h.add(100)
      h.full?.must_equal true
    end

    h.delete_top
    h.full?.must_equal false
    h.add(100)
    h.full?.must_equal true

    5.times do
      h.delete_top
      h.full?.must_equal false
    end
  end

end
