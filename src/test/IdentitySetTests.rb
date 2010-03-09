require 'test/unit'

class IdentitySetTests < Test::Unit::TestCase

  def setup
    @set = IdentitySet.new
    10.times {|i| @set << i}
  end

  def test_enumerable_methods
    assert_equal([0,1,2,3], @set.select    {|i| i < 4},          "select")
    assert_equal(3,         @set.find      {|i| i == 3 },        "find")
    assert_equal(3,         @set.detect    {|i| i == 3 },        "detect")
    assert_equal(5,         @set.count     {|i| (i % 2) == 0 },  "count")
    assert_equal([8,9],     @set.find_all  {|i| i > 7 },         "find_all")
    assert_equal([0],       @set.reject    {|i| i > 0},          "reject")
    assert_equal(10,        @set.inject(0) {|memo,i| memo + 1 }, "inject")
    assert_equal(false,     @set.all?      {|i| i < 7},          "all?")
    assert_equal(true,      @set.any?      {|i| i < 7},          "any?")
  end
end
