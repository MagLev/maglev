# Rake tasks to control the MagLev server.

namespace :maglev do

  desc "Start MagLev server processes, if not already running."
  task :start => [:initialize, :startserver]

  desc "Start the MagLev processes with verbose output."
  task :'start-debug' => [:initialize, :'startserver-debug']

  desc "Start the MagLev processes with performance optimizations."
  task :'start-bench' => [:initialize, :'startserver-bench']

  desc "Stop the MagLev processes."
  task :stop => [:stopserver]

  desc "Restart MagLev server processes."
  task :restart => [:stopserver, :startserver]

  desc "Display MagLev server status."
  task :status do
    status
  end

  desc "Stop MagLev server, overwrite with empty repository!!!"
  task :'force-reload' => [:stopserver, :destroy, :initialize, :start]

  # ======================= core tasks =======================

  task :startserver => :gemstone do
    if server_running?
      puts "Server already running"
    else
      start_netldi
      start_server
      ensure_prims_loaded
    end
  end

  # This just boots the server, but does NOT call ensure prims.  This is
  # useful for starting from a raw image before the mcz is loaded...
  task :bootserver => :gemstone do
    start_netldi
    start_server
  end

  task :'startserver-debug' => :gemstone do
    if server_running?
      puts "Server already running: try stopping server before starting debug"
    else
      start_netldi_debug
      start_server_debug
      ensure_prims_loaded
    end
  end

  task :'startserver-bench' => :gemstone do
    if server_running?
      puts "Server already running: try stopping server before starting bench"
    else
      start_netldi
      start_server_bench
      ensure_prims_loaded
    end
  end

  task :stopserver => :gemstone do
    if server_running?
      stop_server
      stop_netldi
    else
      puts "MagLev Server is not running."
    end
  end

  # TODO: should this target also load an mcz and the primitives?
  task :initialize => :gemstone do
    cd MAGLEV_HOME do
      mkdir_p %w(data/gs64stone/extent data/gs64stone/tranlog log/gs64stone locks)
      install("#{GEMSTONE}/bin/extent0.ruby.dbf", "data/gs64stone/extent", :mode => 0660) unless
        File.exists?("#{MAGLEV_HOME}/data/gs64stone/extent/extent0.ruby.dbf")
    end
  end

  # RxINC: Should this be in a clobber target?
  task :destroy => [:gemstone, :stopserver, :remove_extents]

  task :remove_extents do
    puts "==>  remove_extents"
    cd MAGLEV_HOME do
      # RxINC: is -r necessary?
      rm_rf FileList.new("data/gs64stone/extent/*dbf", "log/gs64stone/*", "locks/*")
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
