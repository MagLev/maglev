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
  task :fav_gems_install => [:shadow_gems_install] do
    puts "== :fav_gems_install: Assuming gems are already clean..."
    cd(File.join(TESTS_DIR, 'favorite_gems')) do
      rm_f 'Gemfile.lock'
      sh "#{MAGLEV_HOME}/bin/bundle install"
    end
  end

  # This tests our custom gem-finding logic. See rubygems/lib/default/maglev.rb
  # This test will break if at any point in the future a bcrypt-ruby-maglev- version 3.0.0
  # is added to rubygems, or either bcrypt-ruby-maglev- or bcrypt-ruby 3.0.1 are yanked.
  task :shadow_gems_install do
    sh "#{MAGLEV_HOME}/bin/maglev-gem install bcrypt-ruby -v'=3.0.1' --no-ri --no-rdoc"
    # Last call fail with a compiler error if our rubygems logic doesn't work
    sh "#{MAGLEV_HOME}/bin/maglev-gem uninstall bcrypt-ruby"
    # Should have uninstalled the gem properly, this is tested with the next line
    unless %x[#{MAGLEV_HOME}/bin/maglev-gem list bcrypt-ruby].strip.empty?
     raise "Failed to uninstall bcrypt-ruby-maglev- when requesting uninstall of bcrypt-ruby"
    end

    # Now we request a version which we have no -maglev- gem for, but we should
    # shadow the original gem completely if MAGLEV_GEMS_ALLOW_ALL is set to false
    msg = %x[MAGLEV_GEMS_ALLOW_ALL=false #{MAGLEV_HOME}/bin/maglev-gem install bcrypt-ruby -v '=3.0.0' --no-ri --no-rdoc]
    unless msg.include? "Could not find a valid gem 'bcrypt-ruby' (= 3.0.0)"
      raise "Failed to shadow original gem with MAGLEV_GEMS_ALLOW_ALL set to false"
    end

    # Now we request a version which we have a -maglev- gem for, and even
    # with MAGLEV_GEMS_ALLOW_ALL set to true, this should shadow the original same version
    msg = %x[MAGLEV_GEMS_ALLOW_ALL=true #{MAGLEV_HOME}/bin/maglev-gem install bcrypt-ruby -v '=3.0.1' --no-ri --no-rdoc]
    unless msg.include? "Successfully installed bcrypt-ruby-3.0.1"
      raise "Failed to shadow an original gem version with MAGLEV_GEMS_ALLOW_ALL set to true"
    end

    sh "#{MAGLEV_HOME}/bin/maglev-gem uninstall bcrypt-ruby"
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
