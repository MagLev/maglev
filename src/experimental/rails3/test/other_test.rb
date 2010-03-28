require 'rubygems'
require 'test/unit'

gem 'i18n'

require 'active_model'
require 'test_person'

# Test that we can include ActiveModel::Translation and make use of it
class TestActiveModelOther < Test::Unit::TestCase
  def test_attribute_methods_basic
    p = TestPerson.new
#    assert_not_nil(p)
  end
end

