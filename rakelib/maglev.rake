# Rake tasks to control the MagLev server.

namespace :maglev do
  desc "Start MagLev server processes, if not already running."
  task :start => :initialize do
    if parser_running?
      puts "Parser already running"
    else
      Rake::Task['maglev:startparser'].invoke
    end
    if server_running?
      puts "Server already running"
    else
      Rake::Task['maglev:startserver'].invoke
    end
  end

  desc "Start the MagLev processes with verbose output."
  task :'start-debug' => [:initialize, :'startparser-debug', :'startserver-debug']

  desc "Start the MagLev processes with performance optimizations."
  task :'start-bench' => [:initialize, :'startparser', :'startserver-bench']

  desc "Stop the MagLev processes."
  task :stop => [:stopserver, :stopparser]

  desc "Restart MagLev server processes."
  task :restart => [:stopserver, :stopparser, :startparser, :startserver]

  desc "Display MagLev server status."
  task :status do
    status
  end

  desc "Stop MagLev server, overwrite with empty repository!!!"
  task :'force-reload' => [:stopserver, :stopparser, :destroy, :initialize, :start]

  # ======================= core tasks =======================

  task :startserver => :gemstone do
    start_netldi
    start_server
    ensure_prims_loaded
  end

  task :'startserver-debug' => :gemstone do
    start_netldi_debug
    start_server_debug
    ensure_prims_loaded
  end

  task :'startserver-bench' => :gemstone do
    start_netldi
    start_server_bench
    ensure_prims_loaded
  end

  task :startparser => :gemstone do
    if parser_running?
      puts "MagLev Parse Server process already running on port #{PARSETREE_PORT}"
    else
      if valid_ruby_for_parser?
        start_parser
      else
        puts "ERROR: #{PARSER_RUBY} won't run the Parse server,"
        puts "       ruby 1.8.6 patchlevel 287 with ParseTree 3.0.3 is required."
        puts "       Upgrade #{PARSER_RUBY} or set the environment variable"
        puts "       RUBY186P287 to point to a ruby 1.8.6 patchlevel 287 executable."
        exit 1
      end
    end
  end

  task :'startparser-debug' => :gemstone do
    start_parser_debug
  end

  task :stopserver => :gemstone do
    if server_running?
      stop_server
      stop_netldi
    else
      puts "MagLev Server is not running."
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
    puts "==>  remove_extents"
    cd MAGLEV_HOME do
      # RxINC: is -r necessary?
      rm_rf FileList.new("data/*dbf", "log/*", "locks/*")
    end
  end

  desc "Run topaz (use rlwrap, if available)"
  task :topaz => :gemstone do
    sh %{ `which rlwrap 2> /dev/null` #{TOPAZ_CMD} }
  end

  desc "Run debug topaz (use rlwrap, if available)"
  task :'topaz-debug' => :gemstone do
    sh %{ `which rlwrap 2> /dev/null` #{TOPAZDEBUG_CMD} }
  end

end
