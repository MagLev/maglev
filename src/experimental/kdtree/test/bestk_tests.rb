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
    bestk.values.must_equal [10,5]

    bestk.add(15)
    bestk.values.must_equal [10,5,15]

    bestk.add(10)
    bestk.values.must_equal [10,5,15,10]

    bestk.add(-3)
    bestk.values.must_equal [10,5,15,10,-3]

    bestk.add(0)
    bestk.values.must_equal [10,5,15,10,0]

    bestk.add(10)
    bestk.values.must_equal [10,5,15,10,10]

    bestk.add(10)
    bestk.values.must_equal [10,10,15,10,10]

    bestk.add(9)
    bestk.values.must_equal [10,10,15,10,10]

    bestk.add(11)
    bestk.values.must_equal [11,10,15,10,10]
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
