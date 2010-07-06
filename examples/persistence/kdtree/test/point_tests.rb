require 'rubygems'
require 'minitest/spec'
require 'tree2d'

MiniTest::Unit.autorun

describe Collections::Point2D do
  before do
    @p1 = Collections::Point2D.new( 1,  2, :foo)
    @p2 = Collections::Point2D.new(10, 20, :bar)
  end

  it 'correctly reports axis values' do
    @p1[0].must_equal 1
    @p1[1].must_equal 2
    @p1.data.must_equal :foo
  end

  it 'raises ArgumentError if index is out of bounds' do
    proc { @p1[2] }.must_raise ArgumentError
    proc { @p1[-1] }.must_raise ArgumentError
  end

  it 'compares eql? correctly' do
    @p1.eql?(@p2).must_equal false
    @p2.eql?(@p1).must_equal false
    p1 = Collections::Point2D.new( 1,  2, :foo)
    @p1.eql?(p1).must_equal true
    [Collections::Point2D.new( 0,  2, :foo),
     Collections::Point2D.new( 1,  0, :foo),
     Collections::Point2D.new( 1,  2, :fooo)].each { |p| p.eql?(@p1).must_equal false }
  end

  it 'calculates distance squared from other points' do
    @p1.distance_sq(@p2).must_equal 405
    @p2.distance_sq(@p1).must_equal 405
    @p1.distance_sq(@p1).must_equal 0
    @p2.distance_sq(@p2).must_equal 0
  end

  it 'if p1.eql?(p2) then p1.hash == p2.hash' do
    100.times do |i|
      x, y, data = rand(1000), rand(1000), rand(1000)
      p1 = Collections::Point2D.new(x, y, data)
      p2 = Collections::Point2D.new(x, y, data)

      p1.eql?(p1).must_equal true
      p1.eql?(p2).must_equal true

      p2.eql?(p1).must_equal true
      p2.eql?(p2).must_equal true

      p1.hash.must_equal p2.hash
    end
  end
end
