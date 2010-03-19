require 'rubygems'
require 'test/unit'

gem 'i18n'

require 'active_model'
require 'xlate_person'

# Test that we can include ActiveModel::Translation and make use of it
class TestActiveModelTranslation < Test::Unit::TestCase
  def test_translation_basic
    p = TranslatePerson.new('fred', 'derf')
    assert_not_nil(p)
    assert_equal('Last name', TranslatePerson.human_attribute_name('last_name'))
    assert_equal('First name', TranslatePerson.human_attribute_name('first_name'))
  end
end

