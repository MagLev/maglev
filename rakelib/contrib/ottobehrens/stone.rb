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

  def Stone.existing(name)
    fail "Stone does not exist" if not GemStoneInstallation.current.stones.include? name
    new(name, GemStoneInstallation.current)
  end

  def Stone.create(name)
    fail "Cannot create stone #{name}: the conf file already exists in #{GemStoneInstallation.current.config_directory}" if GemStoneInstallation.current.stones.include? name
    instance = new(name, GemStoneInstallation.current)
    instance.initialize_new_stone
    instance
  end

  def initialize(name, gemstone_installation, username="DataCurator", password="swordfish")
    @name = name
    @username = username
    @password = password
    @log_directory = "#{gemstone_installation.base_log_directory}/#@name"
    @data_directory = "#{gemstone_installation.installation_extent_directory}/#@name"
    @backup_directory = gemstone_installation.backup_directory
    @extent_name = gemstone_installation.initial_extent_name
    @gemstone_installation = gemstone_installation ||= GemStoneInstallation.current
    initialize_gemstone_environment
  end

  def initialize_gemstone_environment
    ENV['GEMSTONE'] = @gemstone_installation.installation_directory
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
    sh "waitstone #@name #{wait_time} 1>/dev/null" do | ok, status |
      return ok
    end
  end

  def status
    if running?
      sh "gslist -clv #@name"
    else
      puts "#@name not running"
    end
  end

  def start
    log_sh "startstone -z #{system_config_filename} -l #{File.join(log_directory, @name)}.log #{@name}"
    running?(10)
    self
  end

  def stop
    log_sh "stopstone -i #{name} #{username} #{password}"
    self
  end

  def restart
    stop
    start
  end

  def full_backup
    result = run_topaz_command("SystemRepository startNewLog")
    tranlog_number = ((/(\d*)$/.match(result.last))[1]).to_i
    fail if tranlog_number < 0

    run_topaz_command("System startCheckpointSync")
    run_topaz_commands("System abortTransaction", "SystemRepository fullBackupCompressedTo: '#{extent_backup_filename_for_today}'")

    # Transform is to remove the leading directory name
    log_sh "tar --transform='s,.*/,,' -zcf #{backup_filename_for_today} #{extent_backup_filename_for_today} #{data_directory}/tranlog/tranlog#{tranlog_number}.dbf"
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

    log_sh "tar -C '#{backup_directory}' -zxf '#{backup_filename(stone_name, for_date)}'"
    log_sh "cp #{backup_directory}/tranlog*.dbf #{data_directory}/tranlog/"
    run_topaz_command("System commitTransaction. SystemRepository restoreFromBackup: '#{extent_backup_filename(stone_name, for_date)}'")
    run_topaz_command("SystemRepository restoreFromCurrentLogs")
    run_topaz_command("SystemRepository commitRestore")
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

  def backup_filename_for_today
    backup_filename(name, Date.today)
  end

  def extent_backup_filename_for_today
    extent_backup_filename(name, Date.today)
  end

  def extent_backup_filename(for_stone_name, for_date)
    "#{backup_filename_prefix(for_stone_name, for_date)}.full.gz"
  end

  def backup_filename(for_stone_name, for_date)
    "#{backup_filename_prefix(for_stone_name, for_date)}.bak.tgz"
  end

  def backup_filename_prefix(for_stone_name, for_date)
    "#{backup_directory}/#{for_stone_name}_#{for_date.strftime('%F')}"
  end

  def run_topaz_command(command)
    run_topaz_commands(command)
  end

  def run_topaz_commands(*commands)
    topaz_commands(["run", commands.join(". "), "%"])
  end

  def log_sh(command_line)
    sh "echo 'SHELL_CMD #{Date.today.strftime('%F %T')}: #{command_line}' >> #{command_logfile}"
    sh "#{command_line} 2>&1 >> #{command_logfile}"
  end

  def initialize_extents
    install(@gemstone_installation.initial_extent, extent_filename, :mode => 0660)
  end

  def topaz_commands(user_commands, login_first=true)
    commands =  ["output append #{topaz_logfile}",
                 "set u #{username} p #{password} gemstone #{name}" ]
    commands <<  "login" if login_first
    commands << ["limit oops 100",
                 "limit bytes 1000",
                 "display oops",
                 "iferror 1 stack",
                 "iferror 2 exit" ]
    commands << user_commands
    commands << ["output pop",
                 "exit" ]
    Topaz.new(self).commands(commands)
  end
end
