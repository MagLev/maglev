require 'test/unit'

class IdentitySetTests < Test::Unit::TestCase

  def setup
    @set = IdentitySet.new
    10.times {|i| @set << i}
  end

  def test_delete
    small = @set.delete_if {|x| x > 0}
    assert_equal(1, small.size)
    assert(small.include?(0))

    result = @set.delete(2)
    assert_same(result, @set) # delete returns self
    assert(! @set.include?(2))
  end

  def test_it_is_a_set
    s = IdentitySet.new
    o = Object.new
    10.times { s << o }
    assert_equal(1, s.size)
    assert(s.include? o)
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

  def test_superset
    big = IdentitySet.with_all([1,2,3])
    mid = IdentitySet.with_all([1,2])
    small = IdentitySet.with_all([1])

    assert(big.superset?(big))
    assert(big.superset?(mid))
    assert(big.superset?(small))

    assert(! small.superset?(big))
    assert(! small.superset?(mid))
    assert(small.superset?(small))


    assert(! big.proper_superset?(big))
    assert(big.proper_superset?(mid))
    assert(big.proper_superset?(small))

    assert(! small.proper_superset?(big))
    assert(! small.proper_superset?(mid))
    assert(! small.proper_superset?(small))
  end

  def test_subset
    big = IdentitySet.with_all([1,2,3])
    mid = IdentitySet.with_all([1,2])
    small = IdentitySet.with_all([1])

    assert(big.subset?(big))
    assert(! big.subset?(mid))
    assert(! big.subset?(small))

    assert(small.subset?(big))
    assert(small.subset?(mid))
    assert(small.subset?(small))


    assert(! big.proper_subset?(big))
    assert(! big.proper_subset?(mid))
    assert(! big.proper_subset?(small))

    assert(small.proper_subset?(big))
    assert(small.proper_subset?(mid))
    assert(! small.proper_subset?(small))
  end
end
