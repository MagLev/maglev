class GemStoneInstallation
  attr_reader :installation_directory, :config_directory, :installation_extent_directory, :base_log_directory, :backup_directory

  def self.current
    self.new("/opt/gemstone/product")
  end

  def initialize(installation_directory, 
                 config_directory="/etc/gemstone",
                 installation_extent_directory="/var/local/gemstone",
                 base_log_directory="/var/log/gemstone",
                 backup_directory="/var/backups/gemstone")

    @installation_directory = installation_directory
    @config_directory = config_directory
    @base_log_directory = base_log_directory
    @installation_extent_directory = installation_extent_directory
    @backup_directory = backup_directory

    ENV['GEMSTONE'] = @installation_directory
    ENV['PATH'] += ":#{ENV['GEMSTONE']}/bin"
  end

  def stones
    Dir.glob("#{config_directory}/*").collect do | full_filename |
      File.basename(full_filename).split(".").first 
    end
  end

  def status
    sh "gslist -clv"
  end

  def stopnetldi
    sh "stopnetldi"
  end

  def startnetldi
    sh "startnetldi -g"
  end

  def initial_extent
    File.join(@installation_directory, "bin", "extent0.dbf")
  end
end
