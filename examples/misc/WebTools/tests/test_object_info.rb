require 'minitest/unit'
require 'webtools/code_browser'

MiniTest::Unit.autorun

module WebTools
  class TestObjectInfo < MiniTest::Unit::TestCase
    def test_to_json
      obj = Object.new
      obj.instance_variable_set(:@foo, "bar")

      json = ObjectInfo.new(obj).to_json

      assert_kind_of(String, json)
      assert_match(/"object_id":\d+/, json)
      assert_match(/"class":"Object"/, json)
      assert_match(/"@foo"/, json)
    end
  end
end
