# Rake tasks for MagLev core developers.
#
# These tasks depend on the conventions used by the GemStone MagLev
# engineering team.

namespace :dev do
  require 'rakelib/dev.rb'

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

#   desc "Load the mcz file ../latest.mcz and commit it."
#   task :loadmcz => ['maglev:startparser', 'maglev:bootserver'] do
#     puts "=== Load .mcz file: #{`ls -l ../latest.mcz`}"
#     run_topaz tc_load_mcz
#   end

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
      run_ptests(stone.name)
    end
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

  desc 'Run p-tests on gs64stone'
  task :'quick-p-tests' do
    run_ptests
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

  desc "Create .rb files for each smalltalk class (lib/ruby/site_ruby/1.8/smalltalk/*)"
  task :stwrappers => ['maglev:start'] do
    run_topaz tc_gen_st_wrappers
  end

  desc "Load the MagLev native parser"
  task :load_native_parser => ['maglev:start'] do
    run_topaz tc_load_native_parser
  end
end
