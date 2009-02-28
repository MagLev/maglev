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

  # Does a ruby context reset, which reloads the primitives
  def reset_ruby_context
    if running?
      run_topaz_commands("RubyContext reset", "RubyContext load")
    end
  end
end
