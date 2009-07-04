require File.join(File.dirname(__FILE__), 'gemstone_installation')
require File.join(File.dirname(__FILE__), 'topaz')

require 'date'

class Stone
  attr_reader :name, :username, :password
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
    ENV['GEMSTONE_NAME'] = name
    ENV['GEMSTONE_LOGDIR'] = log_directory
    ENV['GEMSTONE_DATADIR'] = data_directory
  end

  # Bare bones stone with nothing loaded, specialise for your situation
  def initialize_new_stone
    mkdir_p @gemstone_installation.config_directory
    create_config_file
    mkdir_p extent_directory
    mkdir_p log_directory
    mkdir_p tranlog_directories
    initialize_extents
  end

  # Will remove everything in the stone's data directory!
  def destroy!
    fail "Can not destroy a running stone" if running?
    rm_rf system_config_filename
    rm_rf extent_directory
    rm_rf data_directory
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
    puts "MagLev server \"#{name}\" starting"
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

  def make_offline_backup
    if running?
      puts "Must stop server before making offline backup."
    else
      puts "Making offline backup to: #{backup_directory}/#{snapshot_filename}"
      log_sh "cd #{extent_directory}; tar zcf #{backup_directory}/#{snapshot_filename} *dbf"
    end
  end

  def restore_offline_backup
    if running?
      puts "Must stop server before restoring full backup."
    else
      rm_rf extent_directory
      rm_rf tranlog_directories
      mkdir_p extent_directory
      mkdir_p tranlog_directories
      puts "Restoring offline backup from: #{backup_directory}/#{snapshot_to_restore}"
      log_sh "cd #{extent_directory}; tar zxfv #{backup_directory}/#{snapshot_to_restore}"
    end
  end

  def full_backup
    result = run_topaz_command("SystemRepository startNewLog")
    tranlog_number = ((/(\d*)$/.match(result.last))[1]).to_i
    fail if tranlog_number < 0

    run_topaz_command("System startCheckpointSync")
    run_topaz_commands("System abortTransaction", "SystemRepository fullBackupCompressedTo: '#{extent_backup_filename_for_today}'")

    log_sh "tar zcf #{backup_filename_for_today} #{extent_backup_filename_for_today} #{data_directory}/tranlog/tranlog#{tranlog_number}.dbf"
  end

  def restore_latest_full_backup
    log_sh "tar -C '#{backup_directory}' -zxf '#{backup_filename_for_today}'"
    run_topaz_command("SystemRepository restoreFromBackup: '#{extent_backup_filename_for_today}'")
    run_topaz_command("SystemRepository restoreFromCurrentLogs")
    run_topaz_command("SystemRepository commitRestore")
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
    File.join(data_directory, "scratch")
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

  def snapshot_filename
    "#{name}_extent.tgz"
    # TODO allow multiple snapshot files by time of day
    # "#{name}_#{Time.now.strftime("%Y%d%H-%H%M")}.bak.tgz"
  end

  def snapshot_to_restore
    "#{name}_extent.tgz"
    # TODO allow selection of snapshot file to restore
  end

  def backup_filename_for_today
    "#{backup_directory}/#{name}_#{Date.today.strftime('%F')}.bak.tgz"
  end

  def extent_backup_filename_for_today
    "#{backup_directory}/#{name}_#{Date.today.strftime('%F')}.full.gz"
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

  def topaz_commands(commands)
    Topaz.new(self).commands("output append #{topaz_logfile}",
                             "set u #{username} p #{password} gemstone #{name}",
                             "login",
                             "limit oops 100",
                             "limit bytes 1000",
                             "display oops",
                             "iferror stack",
                             commands,
                             "output pop",
                             "exit")
  end
end
