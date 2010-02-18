
require 'test/unit'
require 'person'

# This tests using select on un-indexed collections
class TestSelect < Test::Unit::TestCase

  NUM = 100

  def setup
    # Create an IdentitySet, but no index
    @people = IdentitySet.new
    NUM.times { @people << (@a_person = Person.random) }
  end

  def teardown
    @people = nil
    @a_person = nil
  end

  def test_one_level_select
    # Pick someone so we can search for a value that exists
    target_age = @a_person.age

    result = @people.select([:age], :==, target_age)
    assert_not_nil(result, "Result should not be nil")
    assert(result.size > 0, "There should be at least one person of target age")
  end

  def test_multi_level_select
    # Pick someone so we can search for a value that exists
    target_age = @a_person.age

    result = @people.select([:age], :==, target_age)
    assert_not_nil(result, "Result should not be nil")
    assert(result.size > 0, "There should be at least one person of target age")
  end
end
