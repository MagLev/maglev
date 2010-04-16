require 'test/unit'
require 'person'
require 'benchmark'

class TestIndex < Test::Unit::TestCase

  NUM = 100

  def setup
    # Create an IdentitySet with an index on last name
    @people = IdentitySet.new
    @people.create_equality_index('@age', Fixnum)
    NUM.times { @people << Person.random }
  end

  def teardown
    # Must remove the index, since they are permanent (their lifetime
    # may be greater than the lifetime of the associated collection).
    @people.remove_equality_index('@age')
    @people = nil
  end

  def test_one_level_search
    elderly = @people.search([:@age], :gte, 55)
    assert_not_nil(elderly, "The result should not be nil")
    assert_operator(elderly.size, :>=, 1, "Ought to be at least one old person")
  end
end
