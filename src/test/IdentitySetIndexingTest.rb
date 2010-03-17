require 'test/unit'

# Define two classes that both have an @id field so we can test that
# collections can hold instances from multiple, unrelated classes.
class Id
  def initialize(id)
    @id = id
  end
end

class Bar
  def initialize(id)
    @id = id
  end
end

class TestBasicIndexSupport < Test::Unit::TestCase
  def setup
    @idset = IdentitySet.new
    @idset.create_index('id', Fixnum)
    @idx = Array.new
    10.times do |i|
      @idx[i] = Id.new(i)
      @idset << @idx[i]
    end
  end

  def teardown
    @idset.remove_all_indexes
  end

  def test_mixed_class_sets
    # Add Bar instances to set with Id instances
    10.times { |i| @idset << Bar.new(i) }
    assert_equal(20, @idset.length, 'Inserted 10 Bar instances')
    id_2 = @idset.search([:id], :eql, 2)
    assert_equal(2, id_2.size)
  end

  def test_comparison_operators
    # The supported comparison operations are :==, :=, :<, :<=, :>, :>=.
    x = @idset.search([:id], :eql, 9)
    assert_equal(1, x.size)
    x.each {|i| puts "---- #{i.inspect}"}
    assert(x.include?(@idx[9]))

    x = @idset.search([:id], :not_eql, 9)
    assert_equal(9, x.size)
    assert(! x.include?(@idx[9]))

    x = @idset.search([:id], :equal, 8)
    assert_equal(1, x.size)
    assert(x.include?(@idx[8]))

    x = @idset.search([:id], :not_equal, 8)
    assert_equal(9, x.size)
    assert(! x.include?(@idx[8]))

    x = @idset.search([:id], :lt, 2)
    assert_equal(2, x.size)
    assert(x.include?(@idx[0]))
    assert(x.include?(@idx[1]))

    x = @idset.search([:id], :lte, 2)
    assert_equal(3, x.size)
    assert(x.include?(@idx[0]))
    assert(x.include?(@idx[1]))
    assert(x.include?(@idx[2]))

    x = @idset.search([:id], :gt, 8)
    assert_equal(1, x.size)
    assert(x.include?(@idx[9]))

    x = @idset.search([:id], :gte, 8)
    assert_equal(2, x.size)
    assert(x.include?(@idx[9]))
    assert(x.include?(@idx[8]))
  end

  def test_raises_error_on_bad_op
    assert_raise ArgumentError do
      @idset.search([:id], :foo, 8)
    end
  end
end

