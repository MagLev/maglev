require 'rubygems'
require 'test/unit'
require 'flexmock/test_unit'

# To get to FileUtils.sh
require 'rake'
verbose(false)

require 'date'

TEST_STONE_NAME = 'unit.test'

class BaseTestCase < Test::Unit::TestCase
  def setup
    clear_stone(TEST_STONE_NAME)
  end

  def clear_stone(stone_name)
    if GemStoneInstallation.current.stones.include? stone_name
      stone = Stone.existing(stone_name)
      stone.stop
      rm stone.system_config_filename
      rm_rf stone.data_directory
    end
  end

  def deny(*args)
    ! assert(args)
  end

  def test_abstract
    # This is an abstract test case
  end
end
