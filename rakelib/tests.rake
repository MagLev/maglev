TESTS_DIR = File.join(MAGLEV_HOME, 'tests')

namespace :tests do
  desc "Run tests in $MAGLEV_HOME/src/test/vmunit.conf (was dev:vm-tests)"
  task :vmunit do
    # Be sure to return the pass/fail status to the shell by doing an exit.
    # Otherwise, rake swallows the exit status
    exit Stone.new(ENV['STONENAME'] || 'maglev').run_string("run\nRubyContext _runVmUnit\n%")
  end

  desc "Run tests in $MAGLEV_HOME/src/test/vmunit2.conf"
  task :vmunit2 do
    test_dir = File.join(ENV['MAGLEV_HOME'], 'src', 'test')
    conf_file = 'vmunit2.conf'
    Dir.chdir(test_dir) do
      raise "Can't find conf file: #{File.join(test_dir, conf_file)}" unless File.exist? conf_file
      File.foreach conf_file do |line|
        line.chomp!
        next if line =~ /^\s*#/ || line =~ /^\s*$/
        puts "\n======= Running #{line}"
        sh "maglev-ruby #{line}"
      end
    end
  end

  desc "Test that many of our favorite rubygems install"
  # NOTE: we used to do a clean of all the gems here, but there were too
  # many interactions with jenkins.  We now assume the calling environment
  # manages how clean our gemdir is.
  task :fav_gems_install do
    puts "== :fav_gems_install: Assuming gems are already clean..."
    cd(File.join(TESTS_DIR, 'favorite_gems')) do
      rm_f 'Gemfile.lock'
      sh "#{MAGLEV_HOME}/bin/bundle install"
    end
  end

  desc "Run the sinatra gem tests under MagLev"
  task :sinatra do
    cd(File.join(TESTS_DIR, 'sinatra')) do
      sh "./test.sh"
    end
  end

  desc "Run tests on the examples directory"
  task :examples do
    sh "cd examples/sinatra ; rake test"
  end
end
