require 'test/unit'

require 'vsd/stat_series'

# Test the SessionStats Class
module VSD
  class TestStatSeries < Test::Unit::TestCase
    def test_sample_diff
      stats = StatSeries.new
      assert_raise do
        stats.diff
      end

      stats.sample
      assert_raise do
        stats.diff
      end

      stats.sample
      assert_nothing_raised do
        stats.diff
      end
    end
  end
  
end
