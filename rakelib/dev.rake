
# Rake tasks for MagLev core developers.
#
# These tasks depend on the conventions used by the GemStone MagLev
# engineering team.

namespace :dev do
  require 'rakelib/dev.rb'

  desc "Stop server, install ../latest*, and reload primitives"
  task :'install-latest' => [:'dev:cpbuild', :'dev:loadmcz', :'dev:ensureprims']
#  task :'install-latest' => [:'dev:install-tgz', :'dev:loadmcz', :'dev:ensureprims']

  desc "Stop current server and install ../latest-product.tgz"
  task :'install-tgz' do
    tgz_file = '../latest-product.tgz'
    raise "Can't find product #{tgz_file}" unless File.exists?(tgz_file)

    Rake::Task[:'dev:ensure_stopped'].invoke
    ensure_std_directories
    rm_current_gemstone

    untar_product_to_gemstone tgz_file
    copy_extent
  end

  desc "Remove the ./gemstone and ./data directories"
  task :clean_runtime_gemstone => :'maglev:stop' do
    #Rake::Task[:'dev:ensure_stopped'].invoke
    if File.directory?('gemstone')
      sh "chmod -R 777 gemstone"
      sh "rm -rf gemstone"
      sh "rm -rf data"
    end
  end

  desc "Copy build results from ../gss64bit_3.0 into ./gemstone and ./data"
#  task :runtime => :clean_runtime_gemstone do
  task :cpbuild do
    # The runtime environment is made up of two aspects: (1) symlinks to the
    # git checkout that I use all the time and (2) a copy of the
    # build-products, so I can run and do another build at the same time.
    sh 'mkdir locks log data'

    puts "== copying product"
    # 2: Copy the build products
    cp_r '../gss64bit_3.0/fast42/gs/product', 'gemstone'
    cp '../gss64bit_3.0/MagLev.MacOSX.fast/version.txt', 'version.txt'
    cp 'gemstone/bin/extent0.ruby.dbf', 'data'
    chmod 0644, 'data/extent0.ruby.dbf'
  end

  desc "Make sure the gemstone server is stopped."
  task :ensure_stopped do
    # We can't depend on the maglev:stop task, as that depends on there
    # being a gemstone/ directory, but it might not exist yet.  But if
    # there *is* a gemstone dir, then we may as well ensure there is no
    # instance running.
    if File.directory?(GEMSTONE)
      puts "=== Stopping the GemStone Server"
      Rake::Task['maglev:stop'].invoke
    else
      puts "=== No GemStone Server running"
    end
  end

  desc "Reload kernel.rb (primitives) and commit it"
  task :reloadprims => ['maglev:start'] do
    puts "=== reload primitives"
    run_topaz tc_reload_prims
  end

  desc "Ensure kernel.rb (primitives) is loaded and committed"
  task :ensureprims => ['maglev:start'] do
    puts "=== ensure primitives loaded"
    run_topaz tc_ensure_prims
  end

  desc "Load the mcz file ../latest.mcz and commit it."
  task :loadmcz => ['maglev:startparser', 'maglev:bootserver'] do
    puts "=== Load .mcz file: #{`ls -l ../latest.mcz`}"
    run_topaz tc_load_mcz
  end

  desc "Run the passing specs and the vm tests"
  task :smoke => [ 'dev:vm-tests', 'dev:passing' ]

  desc "Run the vm smoke tests"
  task :'vm-tests' do
    run_topaz tc_run_vmunit
    puts "Log files in log/vmunit*"
  end

  desc "Run the passing specs"
  task :'passing' do
    sh "spec/mspec/bin/mspec run -B passing.mspec"
    puts "Log files in log/vmunit*"
  end

  desc "Run the bm smoke tests"
  task :'bm-tests' do
    run_topaz tc_run_benchmarks
    puts "Log files in log/bench*"
  end

  # Run block after ensuring a fresh stone has been created and started
  # The block is passed the stone (e.g., to get the stone name).  The stone
  # will be shutdown and destroyed
  def with_fresh_stone(stone_prefix='TempStone', destroy_on_finish=true)
    stone = nil
    begin
      stones = GemStoneInstallation.current.stones
      stone_name = stone_prefix
      serial = 0
      while stones.include? stone_name
        serial += 1
        stone_name = "#{stone_prefix}#{serial}"
      end
      puts "=== Creating:  #{stone_name}"
      stone = MagLevStone.create(stone_name)
      puts "=== Starting:  #{stone_name}"
      stone.start
      puts "=== Yielding block"
      yield(stone)
      puts "=== block return"
    ensure
      unless stone.nil?
        puts "=== Stopping:  #{stone_name}"
        stone.stop
        if destroy_on_finish
          puts "=== Destroying:  #{stone_name}"
          stone.destroy!
        end
      end
    end
  end

  desc "Run the persistence tests on stone_name (default ptest). Create stone if it does not exist."
  task :'p-tests', :stone_name do
    with_fresh_stone('ptest') do |stone|
      pdir = "#{MAGLEV_HOME}/src/test/persistence/"
      stone_name = stone.name
      ['persistence_tests.rb', 'run_tests.rb', 'run_checks.rb'].each do |fname|
        puts
        puts "=================== #{fname} ======================="
        sh "maglev-ruby --stone #{stone_name} #{pdir}/#{fname}"
      end
    end
  end

  desc "Clean up after a test install of rubygems"
  task :'clean-gems' do
    files = FileList.new('bin/maglev-gem', 'lib/maglev') do |fl|
      fl.include('lib/ruby/site_ruby/1.8/*ubygems.rb')
      fl.include('lib/ruby/site_ruby/1.8/ubygems')
      fl.include('lib/ruby/site_ruby/1.8/rubygems/**/*.rb')
      fl.include('lib/ruby/site_ruby/1.8/rbconfig')

      fl.exclude('lib/ruby/site_ruby/1.8/rubygems/defaults/*')
    end
    files.each { |fn| rm_r fn rescue nil }
  end

  desc "Save a snapshot: Stop the server, copy data/* to snapshot, restart the server"
  task :snapshot => [:'maglev:stopserver', :'dev:takesnapshot', :'maglev:startserver']

  desc "Restore to snapshot: Stop the server, copy snapshot/* to data, restart the server"
  task :tosnapshot => [:'maglev:stopserver', :'dev:tosnapshot', :'maglev:startserver']

  task :takesnapshot do
    if server_running?
      puts "Must stop server before calling :takesnapshot"
    else
      puts "Copying data from data -> snapshot"
      Dir.mkdir('snapshot') unless File.directory?('snapshot')
      cd MAGLEV_HOME do
        sh "cp data/* snapshot"
      end
    end
  end

  task :tosnapshot do
    if server_running?
      puts "Must stop server before calling :tosnapshot"
    else
      puts "Copying data from snapshot -> data"
      cd MAGLEV_HOME do
        sh "cp snapshot/* data"
      end
    end
  end

  desc "Create .rb files for each smalltalk class (lib/ruby/site_ruby/1.8/smalltalk/*)"
  task :stwrappers => ['maglev:start'] do
    run_topaz tc_gen_st_wrappers
  end

  desc "Load the MagLev native parser"
  task :load_native_parser => ['maglev:start'] do
    run_topaz tc_load_native_parser
  end

end
