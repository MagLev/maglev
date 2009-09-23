#!/usr/bin/ruby

require File.join(File.dirname(__FILE__), "..", 'stone')
require File.join(File.dirname(__FILE__), 'common_test_case')

class StoneTestCase < BaseTestCase
end

class StoneUnitTestCase < StoneTestCase
  def test_full_backup
    stone = Stone.create(TEST_STONE_NAME)
    partial_mock_stone = flexmock(stone)

    partial_mock_stone.should_receive(:topaz_commands).with(/System startCheckpointSync/).and_return(["[4202 sz:0 cls: 74241 Boolean] true"]).once.ordered
    partial_mock_stone.should_receive(:topaz_commands).with(/SystemRepository startNewLog/).and_return(["[4202 sz:0 cls: 74241 SmallInteger] 1313"]).once.ordered
    expected_backup_file_name = "#{stone.backup_directory}/#{stone.name}_#{Date.today.strftime('%F')}.full"
    partial_mock_stone.should_receive(:topaz_commands).with(/System abortTransaction. SystemRepository fullBackupCompressedTo: '#{expected_backup_file_name}'/).once.ordered
    
    stone.full_backup
  end

  def test_restore_latest_backup
    stone = Stone.create(TEST_STONE_NAME)
    mock_out_restore_commands(stone, stone.backup_filename_for_today)
    stone.restore_latest_full_backup
  end

  def test_restore_backup_of_another_stone_on_date
    stone = Stone.create(TEST_STONE_NAME)
    mock_out_restore_commands(stone, "#{stone.backup_directory}/anotherstone_2005-05-13.full.gz")
    stone.restore_full_backup('anotherstone', Date.civil(2005, 5, 13))
  end

  def test_run_topaz_file
    stone = Stone.create(TEST_STONE_NAME)
    partial_mock_stone = flexmock(stone)
    partial_mock_stone.should_receive(:topaz_commands).with(["input test.gs", "commit"], true).once.ordered

    stone.input_file("test.gs")
  end

  def test_parse_tranlog_number
    assert(3 == Stone.tranlog_number_from("[18446744073709551610 sz:0 cls: 74241 SmallInteger] 3"))
    assert(-1 == Stone.tranlog_number_from("[18446744073709551610 sz:0 cls: 74241 SmallInteger] -1 == 0xffffffffffffffff"))
  end

  private
  
  def mock_out_restore_commands(stone, backup_filename)
    partial_mock_stone = flexmock(stone)

    partial_mock_stone.should_receive(:recreate!).once.ordered
    partial_mock_stone.should_receive(:topaz_commands).with(/System abortTransaction. SystemRepository restoreFromBackup: '#{backup_filename}'.*/).once.ordered
    partial_mock_stone.should_receive(:topaz_commands).with(/SystemRepository commitRestore/).and_return('commitRestore succeeded').once.ordered
  end
end

class StoneIntegrationTestCase < StoneTestCase

  def delete_existing_backup(stone)
    File.delete stone.backup_filename_for_today if File.exist? stone.backup_filename_for_today
  end

  def test_full_backup
    stone = Stone.create(TEST_STONE_NAME)
    delete_existing_backup(stone)
    stone.start
    stone.full_backup
    assert File.exist? stone.backup_filename_for_today
  end

  def test_restore
    stone = Stone.create(TEST_STONE_NAME)
    delete_existing_backup(stone)
    stone.start
    random_method_name = add_restore_worked_in_userglobals(stone)
    stone.full_backup
    stone.restore_latest_full_backup
    assert_nothing_raised { stone.run_topaz_command("UserGlobals at: ##{random_method_name}") }
  end

  def test_netldi
    GemStoneInstallation.current.stopnetldi
    deny GemStoneInstallation.current.netldi_running?
    GemStoneInstallation.current.startnetldi
    assert GemStoneInstallation.current.netldi_running?
    GemStoneInstallation.current.stopnetldi
    deny GemStoneInstallation.current.netldi_running?
  end

  def test_create_new
    assert !GemStoneInstallation.current.stones.include?(TEST_STONE_NAME)
    stone = Stone.create(TEST_STONE_NAME)
    assert GemStoneInstallation.current.stones.include?(TEST_STONE_NAME)
    stone.start
    assert stone.running?
    assert File.directory?(stone.log_directory)
  end

  def test_destroy
    stone = Stone.create(TEST_STONE_NAME)
    stone.start
    assert_raises(RuntimeError) { stone.destroy! }
    stone.stop
    stone.destroy!
    assert ! (File.exist? stone.extent_directory)
  end

  def test_existing_stone_retrieve
    Stone.create TEST_STONE_NAME
    stone = Stone.existing TEST_STONE_NAME
    assert_not_nil stone
    stone.start
    assert stone.running?
  end

  def test_restart
    stone = Stone.create TEST_STONE_NAME
    stone.start
    stone.restart
    assert stone.running?
    # restart can be a nop and this will still pass. How do I test it? Change a config & restart?
    stone.stop
    stone.restart
    assert stone.running?
  end

  def test_stop_not_running
    stone = Stone.create TEST_STONE_NAME
    stone.stop
    assert !stone.running?
    stone.stop
    assert !stone.running?
  end

  def test_start_already_running
    stone = Stone.create TEST_STONE_NAME
    stone.start
    assert stone.running?
    stone.start
    assert stone.running?
  end

  def test_create_config_file
    config_filename = "#{GemStoneInstallation.current.config_directory}/#{TEST_STONE_NAME}.conf"
    assert ! (File.exist? config_filename)

    stone = Stone.new(TEST_STONE_NAME, GemStoneInstallation.current).create_config_file

    assert File.exists? config_filename
    assert GemStoneInstallation.current.stones.include?(TEST_STONE_NAME)

    content = File.open(config_filename).readlines.join 
    assert content.include? "DBF_EXTENT_NAMES = #{stone.extent_filename}"
    assert content.include? "DBF_SCRATCH_DIR = #{stone.scratch_directory}"
    assert content.include? "STN_TRAN_LOG_DIRECTORIES = #{stone.tranlog_directories.join(",")}"
  end

  def test_stones_in_different_installations_uses_correct_environment
    fake_previous_stone_name = "#{TEST_STONE_NAME}_previous"
    ln_sf GemStoneInstallation.current.installation_directory, "/tmp/fake_previous"
    clear_stone(fake_previous_stone_name)

    current = GemStoneInstallation.current
    previous = GemStoneInstallation.new('/tmp/fake_previous')
    current_stone = Stone.create(TEST_STONE_NAME)
    previous_stone = Stone.create(fake_previous_stone_name, previous)

    assert current.installation_directory != previous.installation_directory
    assert current_stone.data_directory != previous_stone.data_directory
    assert current_stone.log_directory != previous_stone.log_directory

    current_stone.running?
    assert ENV['GEMSTONE'] == current.installation_directory
    assert ENV['GEMSTONE_DATADIR'] == current_stone.data_directory
    assert ENV['GEMSTONE_LOGDIR'] == current_stone.log_directory

    previous_stone.running?
    assert ENV['GEMSTONE'] == previous.installation_directory
    assert ENV['GEMSTONE_DATADIR'] == previous_stone.data_directory
    assert ENV['GEMSTONE_LOGDIR'] == previous_stone.log_directory
  end

  private
  
  def add_restore_worked_in_userglobals(stone)
    random_key = "restoreWorked#{rand 1000}"
    stone.topaz_commands([
                          ["run",
                           "UserGlobals at: ##{random_key} put: 'yay'",
                           '%', 'commit'].join("\n"),
                          ])
    return random_key
  end

end
