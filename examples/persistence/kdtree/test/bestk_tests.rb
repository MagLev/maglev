require 'rubygems'
require 'minitest/spec'
require 'heap'

MiniTest::Unit.autorun

describe Collections::BestK do
  it 'returns correct number of values' do
    # For future implementations, should sort the arrays for comparison.
    k = 5
    bestk = Collections::BestK.new(k)
    bestk.values.must_be_empty
    [[10, [10]],
     [5,  [5,10]],
     [15, [5,10,15]],
     [10, [5,10,10,15]],
     [-3, [-3,5,10,10,15]],
     [0,  [0,5,10,10,15]],
     [10, [5,10,10,10,15]],
     [10, [10,10,10,10,15]],
     [9,  [10,10,10,10,15]],
     [11, [10,10,10,11,15]]].each do |el, result|
      bestk.add(el)
      bestk.values.sort.must_equal result
    end
  end

  it 'maintains the worst element' do
    bestk = Collections::BestK.new(5)
    100.times do
      bestk.add(rand(100))
      worst = bestk.values.min  # search for worst
      bestk.worst.must_equal worst
    end
  end

  it 'does not grow beyond N elements' do
    n = 20
    h = Collections::BestK.new(n)
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

  it 'maintains smallest N if cmp is <' do
    n = 20
    h = Collections::BestK.new(n) {|a,b| a < b}
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
    h = Collections::BestK.new(n) {|a,b| a > b}
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
