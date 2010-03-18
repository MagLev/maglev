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

  def test_search_comparison_operators
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


  def test_search_between
    x = @idset.search_between([:id], 0, 1)
    assert_equal(1, x.size)
    assert(x.include?(@idx[0]))

    x = @idset.search_between([:id], 3, 5)
    assert_equal(2, x.size)
    assert(x.include?(@idx[3]))
    assert(x.include?(@idx[4]))

    x = @idset.search_between([:id], 3, 5, :lt, :lte)
    assert_equal(2, x.size)
    assert(x.include?(@idx[4]))
    assert(x.include?(@idx[5]))

    x = @idset.search_between([:id], 3, 5, :lt, :lte)
    assert_equal(2, x.size)
    assert(x.include?(@idx[4]))
    assert(x.include?(@idx[5]))

    x = @idset.search_between([:id], 3, 5, :lte, :lte)
    assert_equal(3, x.size)
    assert(x.include?(@idx[3]))
    assert(x.include?(@idx[4]))
    assert(x.include?(@idx[5]))

    x = @idset.search_between([:id], -5, -10, :lt, :lte)
    assert_equal(0, x.size)

    x = @idset.search_between([:id], -15, -10, :lt, :lte)
    assert_equal(0, x.size)

    x = @idset.search_between([:id], 15, 20, :lt, :lte)
    assert_equal(0, x.size)
  end
end

# Right now, there is no selction/index support for ruby Time objects.
# But, we can tell it to search/index on the @microseconds (since epoch,
# UTC).  There is also the __microsecs() method on Time objects, so we can
# get at it from Ruby as well.  This is a temporary workaround.t
#
class TestRubyTime < Test::Unit::TestCase
  class Foo
    attr_reader :time
    def initialize(time=Time.now)
      @time = time
    end
  end

  def test_date
    times = [a = Foo.new(Time.gm(2000, "jan", 1, 0, 0, 0)),
             b = Foo.new(Time.gm(2001, "jan", 1, 0, 0, 0)),
             c = Foo.new(Time.gm(2002, "jan", 1, 0, 0, 0))]
    set = IdentitySet.with_all times

    assert(a.time < b.time)
    assert_equal(3, set.size)

    x = set.search([:time,:microseconds], :lt, b.time.__microsecs)
    assert_equal(1, x.size)
    assert(set.include?(a))
  end
end

# Overrides of equality operators not yet supported...
#
# class TestRubySemantics < Test::Unit::TestCase
#   # This class overrides the equality methods to ensure ruby versions are
#   # used.
#   class Quux
#     attr_reader :name

#     def initialize(name)
#       @name = name
#     end

#     # Compare on second letter of @name
#     def <=>(other)
#       @name[1] <=> other.name[1]
#     end
#   end

#   def setup
#     @qs = IdentitySet.new
#     %w(aa ab ac ad ba bb bc bd).each {|n| @qs << Quux.new(n) }
#   end

#   def teardown
#     @qs = nil
#   end

#   def test_quux_comparison
#     a = Quux.new('aaa')
#     b = Quux.new('aba')
#     c = Quux.new('zaz')
#     d = Quux.new('aaa')

#     assert_equal(0, a <=> a)
#     assert_equal(0, a <=> c)
#     assert_equal(0, a <=> d)
#     assert_equal(0, d <=> a)

#     assert_equal(-1, a <=> b)
#     assert_equal(1,  b <=> a)
#   end

#   def test_comparison_override
#     assert_equal(8, @qs.size)  # we have all elements

#     a = @qs.search([:name], :lt, 'ac') # expect: 'aa' 'ab' 'ba' 'bb'
#     p a
#     assert_equal(4, a.size)
#   end
# end
