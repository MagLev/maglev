# Define tasks to control basic GemStone processes (i.e., tasks useful for
# any GemStone install, not just MagLev).

namespace :gs do
  desc "Start GemStone server processes."
  task :start => [:initialize, :startserver, :startparser ]

  desc "Force parser process to start if it's dead."
  task :'start-parser' => [:initialize, :forceparser]

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
    cd MAGLEV_HOME do
      mkdir_p %w(data log locks)
      install("#{GEMSTONE}/bin/extent0.ruby.dbf", "data", :mode => 0660) unless
        File.exists?("#{MAGLEV_HOME}/data/extent0.ruby.dbf")
    end
  end

  # RxINC: Should this be in a clobber target?
  task :destroy => [:initenv, :stopserver] do
    cd MAGLEV_HOME do
      # RxINC: is -r necessary?
      rm_rf FileList.new("data/*dbf", "log/*", "locks/*")
    end
  end

  desc "Run topaz (use rlwrap, if available)"
  task :topaz => :initenv do
    sh %{ `which rlwrap` #{TOPAZ_CMD} }
  end

  desc "Run debug topaz (use rlwrap, if available)"
  task :'topaz-debug' => :initenv do
    sh %{ `which rlwrap` #{TOPAZDEBUG_CMD} }
  end

end
