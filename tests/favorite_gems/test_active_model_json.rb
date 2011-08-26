# This tests basic JSON serialization using active model
require 'rubygems'
require 'bundler/setup'

require 'minitest/unit'
require 'active_model'

MiniTest::Unit.autorun

# Don't name the class "Person", as that sometimes gets committed in other
# tests (e.g., under examples/*)
class AMTestPerson
  include ActiveModel::Serializers::JSON

  attr_accessor :name

  def attributes
    @attributes ||= { 'name' => 'nil'}
  end
end

class TestActiveModelJSONSerialization < MiniTest::Unit::TestCase
  def test_basic
    p = AMTestPerson.new
    p.name = "Bob"
    assert_equal({"person"=>{"name"=>"Bob"}}, p.as_json)
  end
end
