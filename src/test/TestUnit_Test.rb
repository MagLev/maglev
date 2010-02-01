require 'test/unit'

class TC_MyTest < Test::Unit::TestCase
  def setup
    @foo = :foo
  end
  def test_setup
    assert(@foo == :foo, 'setup not called')
  end
  def test_assert
    assert(true, 'Assert true')
  end
  def test_assert_equal
    assert_equal(:foo, @foo, 'assert_equal @foo')
  end
  def teardown
    @foo = nil
  end
end

class TC_MyTest2 < Test::Unit::TestCase
  def setup
    @foo = :foo
  end
  def test_two
    assert(true, 'assert true 2')
  end
  def test_assert_raise
    assert_raise(RuntimeError) { raise RuntimeError.new }
  end
  def test_assert_nothing_raised
    assert_nothing_raised() { "hello" }
  end
  def test_assert_not_nil
    assert_not_nil(@foo, 'assert not nil')
  end
end
