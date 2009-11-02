require 'rubygems'
require 'minitest/spec'
require 'point'

MiniTest::Unit.autorun

describe Point do
  before do
    @p1 = Point.new( 1,  2, :foo)
    @p2 = Point.new(10, 20, :bar)
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
    p1 = Point.new( 1,  2, :foo)
    @p1.eql?(p1).must_equal true
    [Point.new( 0,  2, :foo),
     Point.new( 1,  0, :foo),
     Point.new( 1,  2, :fooo)].each { |p| p.eql?(@p1).must_equal false }
  end
#   it 'calculates distance from other points' do
#     @p1.distance_from(@p2, 0).must_equal
#   end
end
