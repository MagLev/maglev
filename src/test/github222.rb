# https://github.com/MagLev/maglev/issues/222 
#
# $@ isn't actually read-only

require 'test/unit'

class Github222Test < Test::Unit::TestCase

  def test_dollar_at_assignable_in_exception
    begin
      0/0
    rescue
      $@ = ["new_backtrace"]
      assert_equal($@, ["new_backtrace"])
    end
  end

  def test_dollar_at_not_assignable_outside_exception
    assert_raise ArgumentError do
      $@ = ["new_backtrace"]
    end
  end

  def test_dollar_at_allows_only_string_arrays
    begin
      0/0
    rescue
      assert_raise TypeError do
        $@ = "new_backtrace"
      end

      assert_raise TypeError do
        $@ = [123]
      end
    end
  end

  def test_dollar_bang_not_assignable
    if RUBY_VERSION == "1.9.3"
      assert_raise NameError do
        $! = Exception.new
      end
    elsif RUBY_VERSION == "1.8.7"
      exc = Exception.new
      $! = exc
      assert_equal $!, exc
    end
  end

  def test_dollar_at_modifyable_inplace
    begin
      0/0
    rescue
      $@ << "last_backtrace_entry"
      assert $@.size > 1
      assert_equal $@.last, "last_backtrace_entry"
    end
  end

  def test_exception_set_backtrace
    exc = Exception.new
    exc.set_backtrace(["new_backtrace"])
    assert_equal(exc.backtrace, ["new_backtrace"])
  end

  def test_exception_set_backtrace_only_string_arrays
    exc = Exception.new
    assert_raise TypeError do
      exc.set_backtrace("new_backtrace")
    end
  end

end


