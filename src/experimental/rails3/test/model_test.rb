require 'rubygems'
require 'test/unit'

gem 'i18n'

require 'active_model'
require 'tperson'
require 'yaml'
# Test that we can include ActiveModel::Validations and make use of it
class TestActiveModelValidations < Test::Unit::TestCase

  def test_valid_and_invalid
    p = TPerson.new('Foo', 'Bar')
    assert(p.valid?)
    assert(! p.invalid?)

    p = TPerson.new('Foo', nil)
    assert(p.invalid?)
    assert(! p.valid?)
  end

  def test_validates_each
    # The person class has a validates_each block that does not allow first
    # or last names that start with 'z'.
    p = TPerson.new('fred', 'flintstone')
    assert(p.valid?)
    p.last_name = 'zoolander'
    assert(p.invalid?)
    errs = p.errors
    assert_equal(["starts with z."], errs[:last_name])
  end
end

