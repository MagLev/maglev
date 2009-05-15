# MagLev Rakefile

require 'rake/clean'
require 'rake/rdoctask'
require 'rakelib/maglev.rb'

require 'rakelib/contrib/stone.rb'
require 'rakelib/contrib/maglev.rb'

verbose false  # turn off rake's chatter about all the sh commands

CLEAN.include('*.out', 'log/vmunit*.out', 'log/all*.out', 'html')
CLOBBER.include('lib/ruby/site_ruby/1.8/smalltalk')

Rake::RDocTask.new do |rd|
  rd.main = "README.rdoc"
  rd.rdoc_files.include('README*', 'docs/*', 'LICENSES.txt')
end

task :default => :'maglev:status'

# This initializes the environment, and then ensures that there is a
# gemstone diretory there.  Needed to pull this out, since some of the
# initialization tasks need to be performed before there is a gemstone dir
# there, but need the ENV var (i.e., need to know where gemstone should
# be).
task :gemstone do
  raise "\nBad GEMSTONE dir: '#{GEMSTONE}'" unless File.directory?(GEMSTONE)
  raise "\nNo etc/maglev.demo.key" unless File.exists?("etc/maglev.demo.key")
end

desc "Run a squeak image"
task :squeak do
  gem_tools = '/Applications/GemTools-3.0.app'
  if File.exists?(gem_tools)
    sh %{ open #{gem_tools} }
  else
    puts "The #{gem_tools} application used by the 'squeak' command was not found on your system."
    puts "To fix this, correct the 'squeak' command in the gemstone script."
  end
end



desc "Stop netldi"
task :stopnetldi do
  GemStoneInstallation.current.stopnetldi
end

desc "Start netldi"
task :startnetldi do
  GemStoneInstallation.current.startnetldi
end
namespace :stone do
  desc "List MagLev stones managed by this Rakefile"
  task :list do
    puts GemStoneInstallation.current.stones.join("\n")
  end

  desc "Create a new stone"
  task :create, :stone_name do |t, args|
    raise ArgumentError, "Task #{t.name} requires a stone name" unless args.stone_name
    puts "Creating #{args.stone_name}"
    MagLevStone.create(args.stone_name)
  end

  desc "Destroy a stone"
  task :destroy, :stone_name do |t, args|
    raise ArgumentError, "Task #{t.name} requires a stone name" unless args.stone_name
    puts "Destroying #{args.stone_name}"
    s = Stone.existing(args.stone_name)
    s.stop
    s.destroy!
  end
end

def task_gemstone(stone, action)
  desc "#{action.to_s} - #{stone.name}"
  task action do
    stone.send(action)
  end
end

GemStoneInstallation.current.stones.each do |stone_name|
  namespace stone_name do
    stone = MagLevStone.new(stone_name, GemStoneInstallation.current)

    [:stop, :start, :restart, :status, :backup,
      :restore_latest_backup, :reset_ruby_context].each do |action|
      task_gemstone(stone, action)
    end
  end
end
