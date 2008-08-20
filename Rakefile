# MagLev Rakefile
#
# Currently, this Rakefile does many, but not all, of the functions in
# bin/gemstone.  Eventually, it will provide all of the functions in
# bin/gemstone, and other MagLev tasks as the need arises.
#
# Ideas for other tasks:
#
# * The rest of the tasks in bin/gemstone (topaz, ruby,...)
# * clean (remove logs etc.)
# * follow-parser: do a tail -f on the parser log
# * git support for typical workflows (see git support in Rubinius Rakefile)
# * allow command line control of the verbosity of the "sh" calls.
#
task :default => :'gs:status'  # TODO: Do we want to leave this as the default?


# So many things depend on the environment, we just make it global
# Set up the proper GemStone environment for the shell and test for a
# good gemstone install.
task :initenv do
  MAGLEV_HOME = ENV['MAGLEV_HOME'] ||= File.expand_path(File.dirname(__FILE__))
  PARSETREE_PORT = ENV['PARSETREE_PORT'] ||= "2001"
  GEMSTONE = "#{MAGLEV_HOME}/gemstone"
  TOPAZ_CMD ="#{GEMSTONE}/bin/topaz -q -I #{MAGLEV_HOME}/etc/.topazini -l "
  TOPAZDEBUG_CMD = "#{GEMSTONE}/bin/topaz -I #{MAGLEV_HOME}/etc/.topazdebugini -l "

  raise "Bad GEMSTONE dir: '#{GEMSTONE}'" unless File.directory?(GEMSTONE)

  ENV['GEMSTONE_GLOBAL_DIR'] = MAGLEV_HOME
  ENV['GEMSTONE_SYS_CONF']   = "#{MAGLEV_HOME}/etc/system.conf"
  ENV['GEMSTONE_LOG']        = "#{MAGLEV_HOME}/log/gs64stone.log"
  ENV['GEMSTONE']            = GEMSTONE

end
