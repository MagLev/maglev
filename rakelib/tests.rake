TESTS_DIR = File.join(MAGLEV_HOME, 'tests')

namespace :gems do
  desc "Remove all installed gems; reset to checked out version of pre-installed gems"
  task :clean do
    rm_rf 'lib/maglev'
    sh "git checkout lib/maglev"
  end
end

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
  task :fav_gems_install => 'gems:clean' do
    cd(File.join(TESTS_DIR, 'favorite_gems')) do
      rm_f 'Gemfile.lock'
      sh "#{MAGLEV_HOME}/bin/bundle install"
    end
  end
end
