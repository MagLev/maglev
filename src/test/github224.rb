require 'test/unit'

class Github224Test < Test::Unit::TestCase
  def test_io_dup
    old = STDOUT.dup
  ensure
    STDOUT.reopen old
  end
end
