# Require management scripts written by Otto Behrens and Danie Roux
require File.join(File.dirname(__FILE__), 'contrib/ottobehrens/stone')

# Set the GemStoneInstallation paths for a default install of MagLev, based
# on $MAGLEV_HOME.
ML = ENV['MAGLEV_HOME']
# installation_directory, config_directory, installation_extent_directory,
# base_log_directory, backup_directory, initial_extent_name
GemStoneInstallation.current = GemStoneInstallation.new(
  "#{ML}/gemstone", "#{ML}/etc/conf.d", "#{ML}/data",
  "#{ML}/log", "#{ML}/backups", 'extent0.ruby.dbf')

class MagLevStone < Stone
  def config_file_template
    File.open(File.dirname(__FILE__) + "/maglev_stone.conf.template").read
  end

  def key_file
    "#{ML}/etc/maglev.demo.key"
  end

  def create_skeleton
    mkdir_p @gemstone_installation.config_directory
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
    stop if running?
    destroy!
    initialize_new_stone
  end

  def start
    # Don't start parser by defult anymore
    # start_parser unless parser_running?
    puts "MagLev server \"#{name}\" starting..."
    super
    ensure_prims_loaded
  end

  def take_snapshot
    if running?
      stop
      make_offline_backup
      start
    else
      make_offline_backup
    end
  end

  def restore_snapshot
    if running?
      stop
      restore_offline_backup
      start
    else
      restore_offline_backup
    end
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
    ENV['GEMSTONE_NAME'] = name
    # Tell gslist and others where the root of the install is.
    ENV['GEMSTONE_GLOBAL_DIR'] = ENV['MAGLEV_HOME']
  end

  # Expensive: throws away the current ruby context, and creates a new one
  # from scratch.  Side-effect is that all primitives are re-read.
  def reset_ruby_context
    if running?
      run_topaz_commands("RubyContext reset", "RubyContext load")
    end
  end

  # Loads the primitives if they haven't been loaded, then commits the
  # transaction.  Does nothing if prims are already loaded.
  def ensure_prims_loaded
    if running?
      if prims_loaded?(@name)
        puts "Kernel already loaded."
      else
        # Prims can't be loaded without parser running
        start_parser unless parser_running?
        puts "Loading Kernel.  This may take a few seconds..."
        input_file("#{GEMSTONE}/upgrade/ruby/allprims.topaz", false)
      end
    end
  end

  def prims_loaded?(name='gs64stone')
    begin
      Topaz.new(self).commands("output append #{topaz_logfile}",
                               "set u DataCurator p swordfish gemstone #{name}",
                               "login",
                               "obj RubyPrimsLoaded",
                               "output pop",
                               "exit")
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
end
