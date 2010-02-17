
require 'test/unit'
require 'address'

class TestAddress < Test::Unit::TestCase

  NUM_ADDRS = 100

  def test_random
    a = Address.random
    assert_not_nil(a, "Address should not be nil")
  end

end
