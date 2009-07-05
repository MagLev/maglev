#!/usr/bin/ruby

require File.join(File.dirname(__FILE__), "..", 'stone')
require File.join(File.dirname(__FILE__), "..", 'topaz')

require File.join(File.dirname(__FILE__), 'common_test_case')

class TopazTestCase < BaseTestCase
  def setup
    super
    @stone = Stone.create(TEST_STONE_NAME)
    @stone.start
    @topaz = Topaz.new(@stone)
  end

  def test_simple_commands
    @topaz.commands(["status", "exit"])
    fail "Output is #{@topaz.output[1]}" if /^Current settings are\:/ !~ @topaz.output.last

    @topaz.commands("status", "exit")
    fail "Output is #{@topaz.output[1]}" if /^Current settings are\:/ !~ @topaz.output.last
  end

  def test_login
    @topaz.commands("set gems #{@stone.name} u DataCurator p swordfish", "login", "exit")
    fail "Output is #{@topaz.output[2]}" if /^successful login/ !~ @topaz.output.last
  end
  
  def test_nested_commands
    @topaz.commands("set gems #{@stone.name} u DataCurator p swordfish",
                    "login",
                    "level 0",
                    ["printit", "| x |", "x := 6 + 4", "%"],
                    "exit")
    fail "Output is #{@topaz.output.last}" if /^10/ !~ @topaz.output.last
  end

  def test_fail
    assert_raises(TopazError) { @topaz.commands(["an invalid command"]) }
  end
end
