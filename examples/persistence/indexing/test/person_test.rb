
require 'test/unit'
require 'person'

class TestPerson < Test::Unit::TestCase
  def test_random
    assert_not_nil(Person.random, "Should not be nil")
  end
end
