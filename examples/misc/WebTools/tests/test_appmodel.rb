require 'minitest/unit'
require 'webtools/appmodel'

MiniTest::Unit.autorun

class TestAppModel < MiniTest::Unit::TestCase
  def test_format_seconds
    app = WebTools::AppModel.new
    [[0,      "00:00:00"],
     [58,     "00:00:58"],
     [60,     "00:01:00"],
     [7261,   "02:01:01"],
     [86400,  "1 day 00:00:00"],
     [875617, "10 days 03:13:37"],
    ].each do |secs, output|
      assert_equal(output, app.format_secs(secs), "#{secs} => #{output}")
    end
  end
end
