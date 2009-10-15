require 'rubygems'
require 'test/unit'
require 'flexmock/test_unit'

# To get to FileUtils.sh
require 'rake'
verbose(false)

require 'date'

TEST_STONE_NAME = 'unit.test'

class BaseTestCase < Test::Unit::TestCase
  def installation
    GemStoneInstallation.current
  end

  def setup
    clear_stone(TEST_STONE_NAME)
  end

  def clear_stone(stone_name)
    if installation.stones.include? stone_name
      stone = Stone.existing(stone_name, installation)
      stone.stop
      stone.destroy!
    end
  end

  def deny(*args)
    ! assert(args)
  end

  def test_abstract
    # This is an abstract test case
  end

  def teardown
    clear_stone(TEST_STONE_NAME)
  end
end
