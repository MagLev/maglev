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

namespace :gs do # TODO: should this namespace be "gs" or "maglev" or ...?
  desc "Start GemStone server processes."
  task :start => [:initialize, :startserver, :startparser ]

  desc "Force parser process to start if it's dead."
  task :'start-server' => [:initialize, :forceparser]

  desc "Start the GemStone processes with verbose output."
  task :'start-debug' => [:initialize, :'startserver-debug', :'startparser-debug']

  desc "Stop the GemStone processes."
  task :stop => [:stopserver, :stopparser]

  desc "Restart GemStone server processes."
  task :restart => [:stopserver, :stopparser, :startserver, :startparser]

  desc "Display GemStone server status."
  task :status => :statusinternal

  desc "Stop GemStone server, overwrite with empty repository!!!"
  task :'force-reload' => [:stopserver, :stopparser, :destroy, :initialize]

  # ======================= core tasks =======================
  #
  # These tasks are core building block tasks.  There is one task for each
  # shell function in bin/gemstone.  It is fine to call them from the
  # command line, but they are not "desc"cribed, since I'm trying to keep
  # "rake -T" clean.

  task :startserver => :initenv do
    sh %{
      ${GEMSTONE}/bin/startnetldi -g &>/dev/null
      ${GEMSTONE}/bin/startstone gs64stone &>/dev/null
      ${GEMSTONE}/bin/waitstone gs64stone &>/dev/null
    }, :verbose => false do |ok, status|
      puts "GemStone server gs64stone started" if ok
    end
  end

  task :'startserver-debug' => :initenv do
    sh %{
      ${GEMSTONE}/bin/startnetldi -g
      ${GEMSTONE}/bin/startstone  -z ${MAGLEV_HOME}/etc/system-debug.conf gs64stone
      ${GEMSTONE}/bin/waitstone gs64stone &>/dev/null
    }, :verbose => false do |ok, status|
      puts "GemStone server gs64stone started in verbose mode" if ok
    end
  end

  task :startparser => :initenv do
    case `lsof -Fp -iTCP:#{PARSETREE_PORT}`
    when ''
      sh %{
        cd $MAGLEV_HOME/bin > /dev/null
        nohup ruby parsetree_parser.rb >$MAGLEV_HOME/log/parsetree.log 2>/dev/null &
        PARSER_PID="$!"
        echo "MagLev Parse Server process $PARSER_PID started on port $PARSETREE_PORT"
      }, :verbose => false
    else
      puts "MagLev Parse Server process already running on port #{PARSETREE_PORT}"
    end
  end

  task :forceparser => :initenv do
    sh %{
      cd $MAGLEV_HOME/bin > /dev/null
      nohup ruby parsetree_parser.rb \
        >$MAGLEV_HOME/log/parsetree.log 2>$MAGLEV_HOME/log/parsetree.err &
      PARSER_PID="$!"
      echo "MagLev Parse Server process $PARSER_PID started on port $PARSETREE_PORT"
      echo "Check \$MAGLEV_HOME/log/parsetree.err if the parser isn't working"
    }, :verbose => false
  end

  task :'startparser-debug' => :initenv do
    case `lsof -Fp -iTCP:#{PARSETREE_PORT}`
    when ''
      sh %{
        cd $MAGLEV_HOME/bin > /dev/null
        nohup ruby parsetree_parser.rb \
          >$MAGLEV_HOME/log/parsetree.log 2>$MAGLEV_HOME/log/parsetree.err &
        PARSER_PID="$!"
        echo "MagLev Parse Server process $PARSER_PID started on port $PARSETREE_PORT in verbose mode"
        echo "Parser logfiles are \$MAGLEV_HOME/log/parsetree.*"
      }, :verbose => false
    else
      puts "MagLev Parse Server process already running on port #{PARSETREE_PORT}"
    end
  end

  task :stopserver => :initenv do
    sh %{
      ${GEMSTONE}/bin/stopstone gs64stone DataCurator swordfish -i
      ${GEMSTONE}/bin/stopnetldi
    }, :verbose => false
  end

  task :stopparser => :initenv do
    sh %{
      if [ ! -z "`lsof -Fp -iTCP:${PARSETREE_PORT}`" ]; then
        kill -9 `lsof -Fp -iTCP:${PARSETREE_PORT} | cut -c2-`
        true  # Protect against non-zero exit status for the "sh" method
      fi
    }, :verbose => false
  end

  task :statusinternal => :initenv do
    sh %{
      echo "MAGLEV_HOME = $MAGLEV_HOME"
      $GEMSTONE/bin/gslist -clv
      if [ ! -z "`lsof -Fp -iTCP:${PARSETREE_PORT}`" ]; then
        echo "MagLev Parse Server port = $PARSETREE_PORT"
        lsof -P -iTCP:${PARSETREE_PORT}
        # if you don't have permission to run lsof, use the following instead
        # netstat -an | grep "[:.]$PARSETREE_PORT " | grep "LISTEN"
      else
        echo "MagLev Parse Server is not running on port $PARSETREE_PORT"
      fi
    }, :verbose => false
  end

  task :initialize => :initenv do
    sh %{
      cp -p #{GEMSTONE}/bin/extent0.ruby.dbf #{MAGLEV_HOME}/data/
      chmod 660 #{MAGLEV_HOME}/data/extent0.ruby.dbf
    }, :verbose => false unless File.exists?("#{MAGLEV_HOME}/data/extent0.ruby.dbf")
  end

  task :destroy => :initenv do
    sh %{
      rm -rf $MAGLEV_HOME/data/*dbf $MAGLEV_HOME/log/* $MAGLEV_HOME/locks/* 
    }, :verbose => false
  end

  # Set up the proper GemStone environment for the shell and test for a
  # good gemstone install.
  task :initenv do
    MAGLEV_HOME = ENV['MAGLEV_HOME'] ||= File.expand_path(File.dirname(__FILE__))
    PARSETREE_PORT = ENV['PARSETREE_PORT'] ||= "2001"
    GEMSTONE = "#{MAGLEV_HOME}/gemstone"

    raise "Bad GEMSTONE dir: '#{GEMSTONE}'" unless File.directory?(GEMSTONE)

    ENV['GEMSTONE_GLOBAL_DIR'] = MAGLEV_HOME
    ENV['GEMSTONE_SYS_CONF']   = "#{MAGLEV_HOME}/etc/system.conf"
    ENV['GEMSTONE_LOG']        = "#{MAGLEV_HOME}/log/gs64stone.log"
    ENV['GEMSTONE']            = GEMSTONE

#    ENV['TOPAZ_CMD'] ="#{GEMSTONE}/bin/topaz -q -I #{MAGLEV_HOME}/etc/.topazini -l "
#    ENV['TOPAZDEBUG_CMD']="#{GEMSTONE}/bin/topaz -I #{MAGLEV_HOME}/etc/.topazdebugini -l "
  end
end
