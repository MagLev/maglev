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

namespace :dev do
  require 'rakelib/dev.rb'

  desc "Stop the server and copy the extent to the arg: rake dev:savestate'[foo]' copies to ../extent0.ruby.dbf_foo"
  # Need to use full ruby syntax to pass args AND have a dependency.  So
  # this one states that :savestate has a dependency on gs:stop, and that
  # it takes a single arg named :name.  Then the args get passed as the
  # second parameter and you use :name as the selector out of args.
  task({ :savestate => :'gs:stop'}, :name)  do |t, args|
    save_file = "../extent0.ruby.dbf_#{args.name}"
    puts "Saving current extent to #{save_file}"
    cp 'data/extent0.ruby.dbf', save_file
  end

  desc "Stop current server and install MagLev build from ENV['PREVIOUS']"
  task :previous do
    # Ensure the PREVIOUS directory exists, and it contains a 'product.tgz'
    # file. Warn if we don't find any .mcz files.
    previous = File.expand_path(ENV['PREVIOUS'] || "#{MAGLEV_HOME}/../PREVIOUS")
    raise ArgumentError, "No PREVIOUS directory found: '#{previous}'" unless
      File.directory? previous
    files = FileList.new("#{previous}/*")

    tgz_file = files.detect { |f| f =~ /\.tgz$/ }
    mcz_file = files.detect { |f| f =~ /\.mcz$/ }
    raise ArgumentError, "Can't find .tgz file" if tgz_file.nil?
    warn "No .mcz file found in #{previous}." if mcz_file.nil?

    puts "=" * 50
    puts "= Installing product:  #{tgz_file}"
    puts "= Installing mcz file: #{mcz_file}"
    puts "=" * 50

    Rake::Task[:'dev:ensure_stopped'].invoke
    ensure_std_directories
    rm_current_gemstone

    untar_product_to_gemstone tgz_file
    copy_extent

    puts "=== Start GemStone Server"
    Rake::Task['gs:start'].invoke
    load_mcz mcz_file
    Rake::Task['gs:status'].invoke
    # TODO: really load mcz files, if necessary
    # TODO: get topaz snippets working and ensure the image is loaded up
    #       with all of the appropriate items (mcz, primitives, etc.)
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

end
