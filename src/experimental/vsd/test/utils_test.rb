require 'test/unit'

require 'vsd/utils'

# Test the Utils Class
module VSD
  class TestUtils < Test::Unit::TestCase
    def test_mapcar_simple
      d1 = ['foo', 1, 20, 30]
      d2 = ['bar', 3,  6, -3]
      e  = ['foo',-2, 14, 33]

      actual = Utils.mapcar(d1, d2) {|x,y| x.kind_of?(Numeric) ? x - y : x}
      assert_equal(e, actual)
    end
  end
end
