# Test miscellaneous things

require 'test/unit'
require 'date'

class MiscTest < Test::Unit::TestCase

  def test_datetime_offset
    # Trac 701 was found because DateTime did not have an offset method
    # This tests for the offset method workaround / fix
    dt = DateTime.new
    assert_nothing_thrown do
      dt.offset
    end
  end
end
