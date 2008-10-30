# Rake tasks for MagLev core developers.
#
# These tasks depend on the conventions used by the GemStone MagLev
# engineering team.  The following assumptions are made:
#
#  ENV['MAGLEV_HOME'] is the path to the top level development directory.
#  Everything is done here.  Typical value is ~/MagLev/maglev-git.
#
#  ENV['PREVIOUS'], ENV['CURRENT'] are the directories holding the previous
#  and current maglev product-nnn.tgz and *.mcz files.  These variables are
#  only used for the "dev:previous" and "dev:current" targets to find the
#  tgz and mcz files to create the maglev product from.
#
# TODO: Should we bother adding checks for a running instance of GemStone
#       on all the tasks that need it, or only the ones that really need
#       it?  E.g., reloadprims will fail in an obvious way if gs not
#       running, so why bother slowing down the dev cycle with a check for
#       a running instance?
namespace :dev do
  require 'rakelib/dev.rb'

  desc "Stop the server and copy the extent to the arg: rake dev:savestate'[foo]' copies to ../extent0.ruby.dbf_foo"
  # Need to use full ruby syntax to pass args AND have a dependency.  So
  # this one states that :savestate has a dependency on gs:stop, and that
  # it takes a single arg named :name.  Then the args get passed as the
  # second parameter and you use :name as the selector out of args.
  #
  # To invoke from the command line:
  #    $ rake dev:savestate'[foo]'
  # will copy data/extent0.ruby.dbf to ../extent0.ruby.dbf_foo.
  #
  # TODO: At some point, should probably have ENV['MAGLEV_IMAGE_CACHE']
  # point to the directory to cache these in.
  task({:savestate => :'gs:stop'}, :name)  do |t, args|
    # TODO: Do I need to save the tranlog too?
    save_file = "../extent0.ruby.dbf_#{args.name}"
    puts "Saving current extent to #{save_file}"
    cp 'data/extent0.ruby.dbf', save_file
  end

  desc "Restore a previously stashed extent0.ruby.dbf (stops server first)."
  # TODO: Hook into $MAGLEV_IMAGE_CACHE (see comment in task savestate)
  task({:restorestate => :'gs:stop'}, :name)  do |t, args|
    restore_file = "../extent0.ruby.dbf_#{args.name}"
    unless File.exists? restore_file
      puts "Couldn't find restore file #{restore_file}"
      return nil
    end

    was_running = server_running?
    Rake::Task['gs:stop'].invoke if was_running

    # TODO: Do I blow away the tranlog in data?  Restore an old one?
    puts "Restoring extent #{restore_file} to data/extent0.ruby.dbf"
    cp restore_file, 'data/extent0.ruby.dbf'

    Rake::Task['gs:start'].invoke if was_running
  end

  desc "Stop server, install ../latest-product.tgz, load ../latest.mcz and reload primitives"
  task :'install-latest' => [:'dev:install-tgz', :'dev:loadmcz', :'dev:reloadprims']

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

  desc "Make sure the gemstone server is stopped."
  task :ensure_stopped do
    # We can't depend on the gs:stop task, as that depends on there being a
    # gemstone/ directory, but it might not exist yet.  But if there *is* a
    # gemstone dir, then we may as well ensure there is no instance
    # running.
    if File.directory?(GEMSTONE)
      puts "=== Stopping the GemStone Server"
      Rake::Task['gs:stop'].invoke
    else
      puts "=== No GemStone Server running"
    end
  end

  desc "Reload kernel.rb (primitives) and commit it"
  task :reloadprims => ['gs:start'] do
    puts "=== reload primitives"
    run_topaz tc_reload_prims
  end

  desc "Load the mcz file ../latest.mcz and commit it."
  task :loadmcz => ['gs:start'] do
    puts "=== Load .mcz file: #{`ls -l ../latest.mcz`}"
    run_topaz tc_load_mcz
  end

# For some reason, I can't get both to run under one target...
#   desc "Run the smoke tests (vm-tests and passing specs)"
#   task :smoke do
#     Rake::Task[:'dev:vm-tests'].invoke
#     Rake::Task[:'dev:specs'].invoke
#   end

  desc "Run the vm smoke tests (depends on ../gss64bit_30/*)"
  task :'vm-tests' do
    run_topaz tc_run_vmunit
    puts "Log files in log/vmunit*"
  end

  desc "Run the bm smoke tests (depends on ../gss64bit_30/*)"
  task :'bm-tests' do
    run_topaz tc_run_benchmarks
    puts "Log files in log/bench*"
  end
end
