require "minitest/spec"
require "magtag/tweet"

MiniTest::Unit.autorun

describe Tweet do
  it 'should have the content it was created with' do
    msg = "Howdy there"
    t = Tweet.new msg
    refute_nil t
    assert_equal msg, t.text
  end
  
  it 'should limit content to 140 chars' do
    t = Tweet.new "ok"
    refute_nil t
    assert_raises(ArgumentError) { t = Tweet.new "*" * 141 }
  end

  it 'should have a (recent) date' do
    t = Tweet.new "whatever"
    refute_nil t.date
    diff = Time.now - t.date
    assert diff <= 1, "Expecting tweet to be recent but was #{diff} seconds ago"
  end

  it 'should properly twitterize dates' do
    t = Tweet.new "x"
    d = t.date
    assert_equal "2 seconds ago",  t.twitterize_date(t.date+2)
    assert_equal "1 minutes ago",  t.twitterize_date(t.date+60)
    assert_equal "58 minutes ago", t.twitterize_date(t.date+(60*58))
    assert_equal "2 hours ago",    t.twitterize_date(t.date+(60*60*2))
    assert_equal "3 days ago",     t.twitterize_date(t.date+(60*60*24*3))
  end
end
