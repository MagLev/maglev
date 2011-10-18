# Require management scripts written by Otto Behrens and Danie Roux
require File.join(File.dirname(__FILE__), 'contrib/ottobehrens/stone')

# Set the GemStoneInstallation paths for a default install of MagLev, based
# on $MAGLEV_HOME, but respect the $GEMSTONE env variable, if set
ML = ENV['MAGLEV_HOME']

GemStoneInstallation.current =
  GemStoneInstallation.new( ENV['GEMSTONE'] || "#{ML}/gemstone", # installation_directory
                            "#{ML}/etc/conf.d",                  # config_directory
                            "#{ML}/data",                        # installation_extent_directory
                            "#{ML}/log",                         # base log directory
                            "#{ML}/backups",                     # backup directory
                            'extent0.ruby.dbf')                  # initial extent name
def (GemStoneInstallation.current).initial_extent
  File.join(ML, "bin", @initial_extent_name)
end

class MagLevStone < Stone
  def config_file_template
    File.open(File.dirname(__FILE__) + "/maglev_stone.conf.template").read
  end

  def key_file
    "#{ML}/etc/maglev.demo.key"
  end

  def create_skeleton
    mkdir_p @gemstone_installation.config_directory
    mkdir_p @gemstone_installation.backup_directory
    super
  end

  # Will remove everything in the stone's data directory!
  def destroy!
    super
    rm_rf data_directory
  end

  # Will remove everything in the stone's data directory!
  def clobber_data!
    fail "Can not clobber data on a running stone" if running?
    rm_rf extent_directory
    rm_rf tranlog_directories
    mkdir_p extent_directory
    mkdir_p tranlog_directories
  end

  def reload
    with_server_stopped do
      destroy!
      initialize_new_stone
    end
  end

  def start(netldiname='gs64ldi')
    ENV['gs64ldi'] = netldiname
    puts "=== Starting with netldiname #{netldiname}"
    super()
    ensure_prims_loaded
  end

  def take_snapshot
    with_server_stopped { make_offline_backup }
  end

  def restore_snapshot
    with_server_stopped { restore_offline_backup }
  end

  def make_offline_backup
    fail "Must stop server before making offline backup." if running?
    puts "Making offline backup to: #{backup_directory}/#{snapshot_filename}"
    Dir.chdir(extent_directory)
    log_sh "tar zcf #{backup_directory}/#{snapshot_filename} *dbf"
  end

  def restore_offline_backup
    fail "Must stop server before restoring full backup." if running?
    clobber_data!
    puts "Restoring offline backup from: #{backup_directory}/#{snapshot_to_restore}"
    Dir.chdir(extent_directory)
    log_sh "tar zxfv #{backup_directory}/#{snapshot_to_restore}"
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

  def initialize_gemstone_environment
    super
    FORK_ENV['GEMSTONE_NAME'] = name
    # Tell gslist and others where the root of the install is.
    FORK_ENV['GEMSTONE_GLOBAL_DIR'] = ENV['MAGLEV_HOME']
  end

  # Loads the primitives if they haven't been loaded, then commits the
  # transaction.  Does nothing if prims are already loaded.
  def ensure_prims_loaded
    if running?
      reload_prims unless prims_loaded?(@name)
    end
  end

  def reload_prims
    start unless running?
    puts "Loading Kernel for #{@name}.  This may take a few seconds..."
    input_file("#{GEMSTONE}/upgrade/ruby/allprims.topaz", false)
  end

  def prims_loaded?(name='maglev')
    begin
      cmds = ["output append #{topaz_logfile}",
              "set u DataCurator p swordfish gemstone #{name}",
              "login",
              "obj RubyPrimsLoaded",
              "output pop",
              "exit"]
      Topaz.new(self).commands(cmds)
    rescue Exception => e
      # Ignore the exception.  If test for obj RubyPrimsLoaded will cause
      # topaz to exit with a non-zero exit status, which is what we want,
      # so that we can return true or false.
    end
    $?.success?
  end

  def topaz_session
    Topaz.new(self).interactive_session
  end

  private

  # Executes the block with the server stopped.  Will
  # restart the server if the server was running when
  # with_server_stopped was called.
  def with_server_stopped
    was_running = self.running?
    stop if was_running
    yield
    start if was_running
  end
end
