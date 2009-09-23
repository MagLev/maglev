require File.join(File.dirname(__FILE__), 'gemstone_installation')
require File.join(File.dirname(__FILE__), 'topaz')

require 'date'

class Stone
  attr_accessor :username, :password

  attr_reader :name
  attr_reader :log_directory
  attr_reader :data_directory
  attr_reader :backup_directory
  attr_reader :extent_name

  def Stone.existing(name, gemstone_installation=GemStoneInstallation.current)
    fail "Stone does not exist" if not gemstone_installation.stones.include? name
    new(name, gemstone_installation)
  end

  def Stone.create(name, gemstone_installation=GemStoneInstallation.current)
    fail "Cannot create stone #{name}: the conf file already exists in #{gemstone_installation.config_directory}" if gemstone_installation.stones.include? name
    instance = new(name, gemstone_installation)
    instance.initialize_new_stone
    instance
  end

  def initialize(name, gemstone_installation=GemStoneInstallation.current, username="DataCurator", password="swordfish")
    @name = name
    @username = username
    @password = password
    @log_directory = "#{gemstone_installation.base_log_directory}/#@name"
    @data_directory = "#{gemstone_installation.installation_extent_directory}/#@name"
    @backup_directory = gemstone_installation.backup_directory
    @extent_name = gemstone_installation.initial_extent_name
    @gemstone_installation = gemstone_installation
    initialize_gemstone_environment
  end

  def initialize_gemstone_environment
    ENV['GEMSTONE_LOGDIR'] = log_directory
    ENV['GEMSTONE_DATADIR'] = data_directory
  end

  # Bare bones stone with nothing loaded, specialise for your situation
  def initialize_new_stone
    create_skeleton
    initialize_extents
  end

  def create_skeleton
    create_config_file
    mkdir_p extent_directory
    mkdir_p log_directory
    mkdir_p tranlog_directories
  end

  # Will remove everything in the stone's data directory!
  def destroy!
    fail "Can not destroy a running stone" if running?
    rm_rf system_config_filename
    rm_rf extent_directory
    rm_rf log_directory
    rm_rf tranlog_directories
  end

  def running?(wait_time = -1)
    gs_sh "waitstone #@name #{wait_time} 1>/dev/null" do | ok, status |
      return ok
    end
  end

  def status
    if running?
      gs_sh "gslist -clv #@name"
    else
      puts "GemStone server \"#@name\" not running."
    end
  end

  def start
    # Startstone can use single or double quotes around the stone name, so check for either (Yucch)
    gs_sh "startstone -z #{system_config_filename} -l #{File.join(log_directory, @name)}.log #{@name} | grep Info]:.*[\\\'\\\"]#{@name}"
    running?(10)
    self
  end

  def stop
    if running?
      gs_sh "stopstone -i #{name} #{username} #{password} 1>/dev/null"
    else
      puts "GemStone server \"#@name\" not running."
    end
    self
  end

  def restart
    stop
    start
  end

  def self.tranlog_number_from(string)
    ((/\[.*SmallInteger\] (-?\d*)/.match(string))[1]).to_i
  end

  def full_backup
    result = run_topaz_command("System startCheckpointSync")
    fail "Could not start checkpoint, got #{result}" if /\[.*Boolean\] true/ !~ result.last
    result = run_topaz_command("SystemRepository startNewLog")
    tranlog_number = Stone.tranlog_number_from(result.last)
    fail "Could not start a new tranlog" if tranlog_number == -1
    run_topaz_commands("System abortTransaction", "SystemRepository fullBackupCompressedTo: '#{backup_filename_prefix_for_today}'")
  end

  def restore_latest_full_backup
    restore_full_backup(name, Date.today)
  end

  def recreate!
    stop
    destroy!
    initialize_new_stone
    start
  end

  def restore_full_backup(stone_name, for_date=Date.today)
    recreate!
    run_topaz_commands("System abortTransaction. SystemRepository restoreFromBackup: '#{backup_filename(stone_name, for_date)}'")
    run_topaz_commands("SystemRepository commitRestore")
  end

  def backup_filename_for_today
    backup_filename(name, Date.today)
  end

  def backup_filename_prefix_for_today
    backup_filename_prefix(name, Date.today)
  end

  def backup_filename(for_stone_name, for_date)
    "#{backup_filename_prefix(for_stone_name, for_date)}.gz"
  end

  def backup_filename_prefix(for_stone_name, for_date)
    "#{backup_directory}/#{for_stone_name}_#{for_date.strftime('%F')}.full"
  end

  def input_file(topaz_script_filename, login_first=true)
    topaz_commands(["input #{topaz_script_filename}", "commit"], login_first)
  end

  def system_config_filename
    "#{@gemstone_installation.config_directory}/#@name.conf"
  end

  def create_config_file
    require 'erb'
    File.open(system_config_filename, "w") do | file |
      file.write(ERB.new(config_file_template).result(binding))
    end
    self
  end

  def config_file_template
    File.open(File.dirname(__FILE__) + "/stone.conf.template").read
  end

  def extent_directory
    File.join(data_directory, "extent")
  end

  def extent_filename
    File.join(extent_directory, extent_name)
  end

  def scratch_directory
    File.join(data_directory, "scratch.")
  end

  def tranlog_directories
    directory = File.join(data_directory, "tranlog")
    [directory, directory]
  end

  def topaz_logfile
    "#{log_directory}/topaz.log"
  end

  def command_logfile
    mkdir_p log_directory
    "#{log_directory}/stone_command_output.log"
  end

  def gemstone_installation_directory
    @gemstone_installation.installation_directory
  end

  def run_topaz_command(command)
    run_topaz_commands(command)
  end

  def run_topaz_commands(*commands)
    topaz_commands(["run", commands.join(". "), "%"])
  end

  def gs_sh(command_line, &block)
    initialize_gemstone_environment
    @gemstone_installation.gs_sh(command_line, &block)
  end

  def log_gs_sh(command_line)
    log_command_line(command_line)
    gs_sh redirect_command_line_to_logfile(command_line)
  end

  def log_sh(command_line)
    log_command_line(command_line)
    sh redirect_command_line_to_logfile(command_line)
  end

  def initialize_extents
    install(@gemstone_installation.initial_extent, extent_filename, :mode => 0660)
  end

  def topaz_commands(user_commands, login_first=true)
    commands = ["output append #{topaz_logfile}",
                "set u #{username} p #{password} gemstone #{name}" ]
    commands << "login" if login_first
    commands.push(
                "limit oops 100",
                "limit bytes 1000",
                "display oops",
                "iferr 1 stack",
                "iferr 2 exit" ,
                user_commands,
                "output pop",
                "exit")
    Topaz.new(self).commands(commands)
  end

  private 

  def log_command_line(command_line)
    File.open(command_logfile, "a") { |file| file.puts "SHELL_CMD #{Date.today.strftime('%F %T')}: #{command_line}" }
  end

  def redirect_command_line_to_logfile(command_line)
    "#{command_line} 2>&1 >> #{command_logfile}"
  end
end
