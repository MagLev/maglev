require 'rubygems'
require 'test/unit'

gem 'i18n'

require 'active_model'
require 'name_person'

# Test that we can include ActiveModel::Translation and make use of it
class TestActiveModelNaming < Test::Unit::TestCase
  def test_naming_basic
    p = NamePerson.new('fred', 'derf')
    assert_not_nil(p)
    assert_equal('Name person', NamePerson.model_name.human)
    assert_equal('NamePerson', NamePerson.model_name)
  end
end

