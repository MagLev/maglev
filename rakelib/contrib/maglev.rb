# Set the GemStoneInstallation paths for a default install of MagLev, based
# on $MAGLEV_HOME.
ML = ENV['MAGLEV_HOME']
GemStoneInstallation.current= GemStoneInstallation.new(
  "#{ML}/gemstone", "#{ML}/etc/stones", "#{ML}/stones", "#{ML}/log", "#{ML}/backups")

