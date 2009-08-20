# Support for the gemstone rake tasks
#
# These methods are the core building blocks used by the tasks in the
# rakefile.  There is (approximately) one method for each shell function in
# bin/gemstone.

# So many GemStone/S 64 processes depend on the environment variables, we
# just make these global.
MAGLEV_HOME = ENV['MAGLEV_HOME'] ||= File.expand_path("..", File.dirname(__FILE__))
STONENAME = ENV['STONENAME'] ||= "maglev"

PARSETREE_PORT = ENV['PARSETREE_PORT'] ||= "2001"
GEMSTONE = ENV['GEMSTONE'] || "#{MAGLEV_HOME}/gemstone"
TOPAZ_CMD ="#{GEMSTONE}/bin/topaz -q -I #{MAGLEV_HOME}/etc/.topazini -l "
TOPAZDEBUG_CMD = "#{GEMSTONE}/bin/topaz -I #{MAGLEV_HOME}/etc/.topazini -l "
IRB_CMD = "$GEMSTONE/bin/topaz -q -I $MAGLEV_HOME/etc/.irbdebugini -l "

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

# RUBY186P287 must be set to a ruby 1.8.6 patchlevel 287 executable
# in order to run Parse Server. Earlier versions will fail.
PARSER_RUBY                = ENV['RUBY186P287'] || "ruby"


def run_topaz(snippet)
  sh %{ #{TOPAZ_CMD} <<EOF
login
#{snippet}
EOF
  } do |ok, status|
    ok
  end
end
