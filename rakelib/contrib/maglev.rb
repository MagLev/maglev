require File.join(File.dirname(__FILE__), 'stone')
# Set the GemStoneInstallation paths for a default install of MagLev, based
# on $MAGLEV_HOME.
ML = ENV['MAGLEV_HOME']
GemStoneInstallation.current= GemStoneInstallation.new(
  "#{ML}/gemstone", "#{ML}/etc/stones", "#{ML}/stones", "#{ML}/log", "#{ML}/backups")

class MagLevStone < Stone
  def config_file_template
    File.open(File.dirname(__FILE__) + "/maglev_stone.conf.template").read
  end

  def key_file
    "#{ML}/etc/maglev.demo.key"
  end

  def initialize_gemstone_environment
    super
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
      run_topaz_commands("RubyContext ensurePrimsLoaded")
    end
  end
end
