# Define tasks to control basic GemStone processes (i.e., tasks useful for
# any GemStone install, not just MagLev).

# RxINC: Need a task to test if gs is already running., then make topaz
# depend on it.

# Ideas for Tasks:
#
# 1: Stash image: stop gs, copy the data/extent0.*, restart gs.  The idea
#    here is that I can load up to a known state and then get back to it
#    when ever.
#
# 2: Revert image: stop gs, copy the stashed image, restart gs.  This is
#    the counterpart to Stash image.
namespace :gs do
  desc "Start GemStone server processes, if not already running."
  task :start => :initialize do
    if server_running?
      puts "Server already running"
    else
      Rake::Task['gs:startserver'].invoke
    end
    if parser_running?
      puts "Parser already running"
    else
      Rake::Task['gs:startparser'].invoke
    end
  end

  desc "Start the GemStone processes with verbose output."
  task :'start-debug' => [:initialize, :'startserver-debug', :'startparser-debug']

  desc "Stop the GemStone processes."
  task :stop => [:stopserver, :stopparser]

  desc "Restart GemStone server processes."
  task :restart => [:stopserver, :stopparser, :startserver, :startparser]

  desc "Display GemStone server status."
  task :status do
    status
  end

  desc "Stop GemStone server, overwrite with empty repository!!!"
  task :'force-reload' => [:stopserver, :stopparser, :destroy, :initialize, :start]

  # ======================= core tasks =======================

  task :startserver => :gemstone do
    start_server
  end

  task :'startserver-debug' => :gemstone do
    start_server_debug
  end

  task :startparser => :gemstone do
    if parser_running?
      puts "MagLev Parse Server process already running on port #{PARSETREE_PORT}"
    else
      start_parser
    end
  end

  task :'startparser-debug' => :gemstone do
    start_parser_debug
  end

  task :stopserver => :gemstone do
    if server_running?
      stop_server
    else
      puts "GemStone Server is not running."
    end
  end

  task :stopparser => :gemstone do
    puts "No parser running on port #{PARSETREE_PORT}" unless stop_parser.nil?
  end

  # TODO: should this target also load an mcz and the primitives?
  task :initialize => :gemstone do
    cd MAGLEV_HOME do
      mkdir_p %w(data log locks)
      install("#{GEMSTONE}/bin/extent0.ruby.dbf", "data", :mode => 0660) unless
        File.exists?("#{MAGLEV_HOME}/data/extent0.ruby.dbf")
    end
  end

  # RxINC: Should this be in a clobber target?
  task :destroy => [:gemstone, :stopserver, :remove_extents]

  task :remove_extents do
    puts "===>  remove_extents"
    cd MAGLEV_HOME do
      # RxINC: is -r necessary?
      rm_rf FileList.new("data/*dbf", "log/*", "locks/*")
    end
  end

  desc "Run topaz (use rlwrap, if available)"
  task :topaz => :gemstone do
    sh %{ `which rlwrap` #{TOPAZ_CMD} }
  end

  desc "Run debug topaz (use rlwrap, if available)"
  task :'topaz-debug' => :gemstone do
    sh %{ `which rlwrap` #{TOPAZDEBUG_CMD} }
  end

end
