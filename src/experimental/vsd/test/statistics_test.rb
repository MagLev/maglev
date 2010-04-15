require 'test/unit'

require 'vsd'

# Test the Statistics Module
class TestStatistics < Test::Unit::TestCase
  def test_spc_stats
    spc_stats = Maglev::Statistics.spc_statistics
    assert_equal('ShrPcMonitor', spc_stats['ProcessName'], 'spc_stats process name')
  end
end
