require 'test/unit'

require 'vsd'

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
  end
end
