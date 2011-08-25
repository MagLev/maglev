# Ensure a proper GEMSTONE/Maglev environment
MAGLEV_HOME = ENV['MAGLEV_HOME'] ||= File.expand_path("..", "..", File.dirname(__FILE__))
STONENAME   = ENV['STONENAME']   ||= "maglev"
GEMSTONE    = ENV['GEMSTONE']    || "#{MAGLEV_HOME}/gemstone"

# Maglev doesn't allow changes to $GEMSTONE* variables during execution
# (i.e., you can't change the stone your connected to, once you've
# connected).  Assume that if $GEMSTONE is set correctly, then all the
# others are too.
if ENV['GEMSTONE'].nil? or ENV['GEMSTONE'].empty?
  ENV['GEMSTONE_GLOBAL_DIR'] = MAGLEV_HOME
  ENV['GEMSTONE_SYS_CONF']   = "#{MAGLEV_HOME}/etc/system.conf"
  ENV['GEMSTONE_DATADIR']    = "#{MAGLEV_HOME}/data/#{STONENAME}"
  ENV['GEMSTONE_LOG']        = "#{MAGLEV_HOME}/log/#{STONENAME}/#{STONENAME}.log"
  ENV['GEMSTONE']            = GEMSTONE
end
