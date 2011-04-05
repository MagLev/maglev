require 'minitest/unit'
require 'webtools/code_browser'

MiniTest::Unit.autorun

class Xyzzy
  attr_reader :foo
  def initialize
    @foo = :foo
  end
end

module WebTools
  class TestObjectInfo < MiniTest::Unit::TestCase
    def test_for_id
      x = Xyzzy.new
      oi = ObjectInfo.for_id(x.object_id)
      refute_nil oi

      info = oi.info
      refute_nil info

      assert_equal x.object_id, info[:object_id]
      assert_equal 'Xyzzy',     info[:class]
      assert_match /#<Xyzzy:0x[0-9a-f]+ @foo=:foo>/, info[:inspect]

      foo_info = info[:instance_variables][0]
      assert_equal '@foo', foo_info[0]  # name
      assert_equal ':foo', foo_info[1]  # val
      assert_equal x.foo.object_id, foo_info[2]

      assert_empty info[:enumerated]
      assert_nil   info[:enumerated_size]
    end

    def test_an_array
      a = [:foo, 1, "twelve"]
      20.times { |i| a << i }

      oi = ObjectInfo.for(a)
      refute_nil oi

      info = oi.info
      refute_nil info

      assert_equal a.object_id, info[:object_id]
      assert_equal 'Array',     info[:class]
      assert_equal "[:foo, 1, \"twelve\", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]", info[:inspect]

      assert_equal [], info[:instance_variables]

      assert_equal 23, info[:enumerated_size]
      assert_equal [":foo", "1", "\"twelve\"", "0", "1", "2", "3", "4", "5", "6", "7", "..."],
                   info[:enumerated]
      
    end
  end
end
