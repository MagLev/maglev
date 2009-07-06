class GemStoneInstallation
  attr_reader :installation_directory, :config_directory, :installation_extent_directory, :base_log_directory, :backup_directory, :initial_extent_name

  @current = nil
  def self.current
    @current ||= self.new("/opt/gemstone/product")
  end

  def self.current=(instance)
    @current = instance
  end

  def initialize(installation_directory,
                 config_directory="/opt/gemstone/etc/conf.d",
                 installation_extent_directory="/opt/gemstone/product/data",
                 base_log_directory="/opt/gemstone/log",
                 backup_directory="/opt/gemstone/backups",
                 initial_extent_name='extent0.dbf')

    @installation_directory = installation_directory
    @config_directory = config_directory
    @base_log_directory = base_log_directory
    @installation_extent_directory = installation_extent_directory
    @backup_directory = backup_directory
    @initial_extent_name = initial_extent_name

    ENV['GEMSTONE'] = @installation_directory
    ENV['PATH'] += ":#{ENV['GEMSTONE']}/bin"
  end

  def stones
    Dir.glob("#{config_directory}/*").collect do | full_filename |
      File.basename(full_filename).split(".conf").first 
    end
  end

  def status
    sh "gslist -clv"
  end

  def stopnetldi
    sh "stopnetldi"
  end

  def startnetldi
    sh "startnetldi -g -a #{ENV['USER']}"
  end

  def initial_extent
    File.join(@installation_directory, "bin", @initial_extent_name)
  end
end
