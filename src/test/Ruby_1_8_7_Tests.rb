# Some tests for 1.8.7 support

require 'test/unit'

class SymbolTest < Test::Unit::TestCase
  def test_to_proc
    # See new pick-axe p. 368
    names = %w{ant bee cat}
    result = names.map(&:upcase)
    assert_equal(result, %w{ANT BEE CAT})
  end
end
