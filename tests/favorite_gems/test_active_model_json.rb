# This tests basic JSON serialization using active model
require 'rubygems'
require 'bundler/setup'

require 'minitest/unit'
require 'active_model'

MiniTest::Unit.autorun

class Person
  include ActiveModel::Serializers::JSON

  attr_accessor :name

  def attributes
    @attributes ||= { 'name' => 'nil'}
  end
end

class TestActiveModelJSONSerialization < MiniTest::Unit::TestCase
  def test_basic
    p = Person.new
    p.name = "Bob"
    assert_equal({"person"=>{"name"=>"Bob"}}, p.as_json)
  end
end
