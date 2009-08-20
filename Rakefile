# MagLev Rakefile

require 'rake/clean'
require 'rake/rdoctask'
require 'rakelib/maglev'
require 'rakelib/parser'

require 'rakelib/maglev_stone.rb'
require 'rakelib/contrib/ottobehrens/stone.rb'

verbose false  # turn off rake's chatter about all the sh commands

CLEAN.include('*.out', 'log/vmunit*.out', 'log/all*.out', 'html', 'vmunit.log', 'topazerrors.log')
CLOBBER.include('lib/ruby/site_ruby/1.8/smalltalk', 'version.txt')

Rake::RDocTask.new do |rd|
  rd.main = "README.rdoc"
  rd.rdoc_files.include('README*', 'docs/*', 'LICENSES.txt', 'src/kernel/bootstrap/Maglev.rb')
end

task :default => :status

desc "Show status of all stones"
task :status do
  Rake::Task['stone:all'].invoke(:status)
end

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
  gem_tools = '/Applications/Maglev.app'
  if File.exists?(gem_tools)
    sh %{ open #{gem_tools} }
  else
    puts "The #{gem_tools} application used by the 'squeak' command was not found on your system."
    puts "To fix this, correct the 'squeak' command in the gemstone script."
  end
end

namespace :stone do
  desc "List MagLev servers managed by this Rakefile"
  task :list do
    puts GemStoneInstallation.current.stones.join("\n")
  end

  desc "Create a new MagLev server and repository"
  task :create, :server_name do |t, args|
    raise ArgumentError, "Task #{t.name} requires a new server name" unless args.server_name
    puts "Creating #{args.server_name}"
    MagLevStone.create(args.server_name)
  end

  desc "Destroy an existing MagLev server and repository"
  task :destroy, :server_name do |t, args|
    raise ArgumentError, "Task #{t.name} requires an existing server name" unless args.server_name
    puts "Destroying #{args.server_name}"
    s = Stone.existing(args.server_name)
    s.stop
    s.destroy!
  end

  desc "Invoke a task on all MagLev servers"
  task :all, :task_name do |t,args|
    GemStoneInstallation.current.stones.each do |server_name|
      Rake::Task["#{server_name}:#{args.task_name}"].invoke
    end
  end
end

namespace :netldi do
  desc "Stop netldi"
  task :stop do
    GemStoneInstallation.current.stopnetldi
  end
  desc "Start netldi"
  task :start do
    GemStoneInstallation.current.startnetldi
  end
end

def task_gemstone(stone, action, desc=nil)
  desc "#{desc.nil? ? action.to_s : desc}"
  task action do
    stone.send(action)
  end
end


GemStoneInstallation.current.stones.each do |server_name|
  namespace server_name do
    stone = MagLevStone.new(server_name, GemStoneInstallation.current)
    [[:start,            "Start the \"#{server_name}\" server"],
     [:stop,             "Stop the \"#{server_name}\" server"],
     [:restart,          "Stop then start the \"#{server_name}\" server"],
     [:status,           "Report status of the \"#{server_name}\" server"],
     [:reload,           "Destroy the \"#{server_name}\" repository then load a fresh one"],
     [:take_snapshot,    "Stop the \"#{server_name}\" server then make a backup copy of its repository"],
     [:restore_snapshot, "Restore the \"#{server_name}\" repository from its previous snapshot"]
    ].each do |action,desc|
      task_gemstone(stone, action, desc)
    end
  end
end
