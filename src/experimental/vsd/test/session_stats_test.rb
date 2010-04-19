require 'test/unit'

require 'vsd/session_stats'

# Test the SessionStats Class
module VSD
  class TestSessionStats < Test::Unit::TestCase
    def test_spc_stats
      spc_stats = SessionStats.spc_statistics
      assert_equal('ShrPcMonitor', spc_stats['ProcessName'], 'spc_stats process name')
    end

    def test_my_statistics
      my_stats = SessionStats.my_statistics
      assert_equal($$, my_stats['ProcessId'], 'my_stats pid')
    end

    def test_accessor
      stats = SessionStats.new
      assert_not_nil(stats)
      assert_kind_of(String, stats['ProcessName'])
    end

    def test_difference
      d1 = Array.new(100) {|i| i}
      d2 = Array.new(100) {|i| i+1}

      s1 = SessionStats.new(1, 'start', d1)
      s2 = SessionStats.new(2, 'stop',  d2)
      diff = nil
      assert_nothing_raised do
        diff = s2 - s1
      end
      assert_not_nil(diff)
      assert( diff.stats.all? {|i| i == 1})
    end
  end
end
