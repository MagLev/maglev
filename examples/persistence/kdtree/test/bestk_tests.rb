require 'rubygems'
require 'minitest/spec'
require 'bestk'

MiniTest::Unit.autorun

describe BestK do
  it 'returns correct number of values' do
    # For future implementations, should sort the arrays for comparison.
    k = 5
    bestk = BestK.new(k)
    bestk.values.must_be_empty

    bestk.add(10)
    bestk.values.must_equal [10]

    bestk.add(5)
    bestk.values.sort.must_equal [5,10]

    bestk.add(15)
    bestk.values.sort.must_equal [5,10,15]

    bestk.add(10)
    bestk.values.sort.must_equal [5,10,10,15]

    bestk.add(-3)
    bestk.values.sort.must_equal [-3,5,10,10,15]

    bestk.add(0)
    bestk.values.sort.must_equal [0,5,10,10,15]

    bestk.add(10)
    bestk.values.sort.must_equal [5,10,10,10,15]

    bestk.add(10)
    bestk.values.sort.must_equal [10,10,10,10,15]

    bestk.add(9)
    bestk.values.sort.must_equal [10,10,10,10,15]

    bestk.add(11)
    bestk.values.sort.must_equal [10,10,10,11,15]
  end

  it 'maintains the worst element' do
    bestk = BestK.new(5)
    100.times do
      bestk.add(rand(100))
      worst = bestk.values.min
      #puts "--- worst: #{worst}  values: #{bestk.values.inspect}  bestk.worst: #{bestk.worst}"
      bestk.worst.must_equal worst
    end
  end
end
