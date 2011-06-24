# Rake tasks for MagLev core developers.
#
# These tasks depend on the conventions used by the GemStone MagLev
# engineering team.

namespace :dev do
  desc "Create some TAGS files"
  task :tags do
    cd('src') { sh %{ /opt/local/bin/ctags -a -e -f TAGS --tag-relative -R * }}
  end

  desc "Run the passing specs and the vm tests"
  task :smoke => [ 'dev:vm-tests', 'spec:ci' ]

  desc "Run the GemTests set of tests"
  task :gemtests do
    cd('src/test/GemTests') do
      sh %{ rake test }
    end
  end

  desc "Run the vm smoke tests"
  task :'vm-tests' => :stwrappers do
    # Be sure to return the pass/fail status to the shell by doing an exit.
    # Otherwise, rake swallows the exit status
    exit Stone.new(ENV['STONENAME'] || 'maglev').run_string("run\nRubyContext _runVmUnit\n%")
  end

  task :'vm2-tests' do
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

  desc "Run maglev-gem pristine on rails gems"
  task :pristinerails do
    %w(actionmailer actionpack activemodel activerecord
       activeresource activesupport arel bundler rails railties).each do |g|
      sh "maglev-gem pristine #{g}"
      echo "Patch maglev binaries shebang line...patch activesupport"
    end
  end
  
  desc "Run the passing specs"
  task :'passing' do
    sh "spec/mspec/bin/mspec run -B passing.mspec"
    puts "Log files in log/vmunit*"
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
      run_ptests(stone.name)
    end
  end

  desc "Run persistence tests on stone per STONENAME env var"
  task :'p-tests-STONENAME' do
    run_ptests(nil)
  end

  def run_ptests(stone_name=nil)
    pdir = "#{MAGLEV_HOME}/src/test/persistence/"
    stone_spec = stone_name.nil? ? "" : "--stone #{stone_name}"
    ['persistence_tests.rb', 'run_tests.rb', 'run_checks.rb'].each do |fname|
      puts
      puts "=================== #{fname} ======================="
      sh "maglev-ruby #{stone_spec} #{pdir}/#{fname}"
    end
  end

  desc 'Run p-tests on maglev'
  task :'quick-p-tests' do
    run_ptests
  end

  desc "Clean up after a test install of rubygems"
  task :'clean-gems' do
    puts "CLEANING GEMS"
    files = FileList.new('bin/maglev-gem', 'lib/maglev') do |fl|
      fl.include('bin/rackup')
      fl.include('bin/rake')
      fl.include('lib/ruby/site_ruby/1.8/*ubygems.rb')
      fl.include('lib/ruby/site_ruby/1.8/ubygems')
      fl.include('lib/ruby/site_ruby/1.8/rubygems/**/*.rb')
      fl.include('lib/ruby/site_ruby/1.8/rbconfig')

      fl.exclude('lib/ruby/site_ruby/1.8/rubygems/defaults/*')
    end
    files.each { |fn| rm_r fn rescue nil }
  end

  desc "Clear out the old rubygems and install a new version"
  task :'new-gems' => 'dev:clean-gems' do
    cd('src/external/rubygems-1.3.5') do
      sh "maglev-ruby ./setup.rb --no-rdoc --no-ri"
    end
    cp "/Users/pmclain/GemStone/dev/maglev-gem", "bin"
  end

  desc "Run topaz (use rlwrap, if available)"
  task :topaz => :gemstone do
    TOPAZ_CMD = "#{GEMSTONE}/bin/topaz -q -I #{MAGLEV_HOME}/etc/.topazini -l "
    sh %{ `which rlwrap 2> /dev/null` #{TOPAZ_CMD} }
  end

  desc "Load the primitives into the default image.  This makes subsequent stone creation faster as prims already loaded."
  task :installprims do
    # Backup the original extent
    cp GemStoneInstallation.current.initial_extent,
       "#{GemStoneInstallation.current.initial_extent}.orig"
    with_fresh_stone do |stone|
      stone.start # loads primitives
      stone.stop
      cp stone.extent_filename, GemStoneInstallation.current.initial_extent
    end
  end
end

# These are dev specific tasks we want on a per stone basis
GemStoneInstallation.current.stones.each do |stone_name|
  namespace stone_name do
    stone = MagLevStone.new(stone_name, GemStoneInstallation.current)

    [[:reload_prims, "Reset the ruby context in \"#{stone_name}\" then reload primitives"]
    ].each do |action, desc|
      task_gemstone(stone, action, " [DEV] #{desc}")
    end
  end
end

desc "Kill the named stone"
task :kill, :stone do |t, args|
  stone = args.stone || 'maglev'
  kill_stone(stone)
end

desc "Kill -9 the named stone"
task :killkill, :stone do |t, args|
  stone = args.stone || 'maglev'
  kill_stone(stone, '-9')
end

def kill_stone(stone, sig='')
  stones = `$GEMSTONE/bin/gslist -clv`
  puts stones
  pids = stones.grep(/(Stone|cache)\s+#{stone}/) { |l| l.split[3] }
  if pids.empty?
    puts "Nothing to kill for #{stone}"
  else
    pids = pids.join(" ")
    puts "Killing #{stone}: #{pids}"
    sh "kill #{sig} #{pids}"
  end
end
