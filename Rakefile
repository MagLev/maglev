# MagLev Rakefile

require 'rake/clean'

# Try to support Rake 0.8.7 and 0.9.x (at least for a while)
if defined? Rake::VERSION
puts "============ #{Rake::VERSION}  NEW"
  # Rake 0.9.x
  require 'rubygems'
  gem 'rdoc'
  require 'rdoc/task'
else
puts "============ OLD"
  # Rake 0.8.x
  require 'rake/rdoctask'
end

$LOAD_PATH << File.dirname(__FILE__)  # For 1.9, '.' is no longer in the load path

require 'rakelib/maglev_stone.rb'
require 'rakelib/contrib/ottobehrens/stone.rb'

MAGLEV_HOME    = ENV['MAGLEV_HOME'] ||= File.expand_path("..", File.dirname(__FILE__))
STONENAME      = ENV['STONENAME']   ||= "maglev"
GEMSTONE       = ENV['GEMSTONE']    || "#{MAGLEV_HOME}/gemstone"

# Maglev doesn't allow changes to $GEMSTONE* variables during execution
# (i.e., you can't change the stone your connected to, once you've
# connected).  Assume that if $GEMSTONE is set correctly, then all the
# others are too.
if ENV['GEMSTONE'].nil? or ENV['GEMSTONE'].empty?
  ENV['GEMSTONE_GLOBAL_DIR'] = MAGLEV_HOME
  ENV['GEMSTONE_SYS_CONF']   = "#{MAGLEV_HOME}/etc/system.conf"
  ENV['GEMSTONE_DATADIR']    = "#{MAGLEV_HOME}/data/#{STONENAME}"
  ENV['GEMSTONE_LOG']        = "#{MAGLEV_HOME}/log/#{STONENAME}/#{STONENAME}.log"
  ENV['GEMSTONE']            = GEMSTONE
end
verbose false  # turn off rake's chatter about all the sh commands

CLEAN.include('*.out', 'log/vmunit*.out', 'log/all*.out', 'html',
              'vmunit.log', 'topazerrors.log', 'gem_*_code.log')
CLOBBER.include('lib/ruby/site_ruby/1.8/smalltalk', 'version.txt')

Rake::RDocTask.new do |rd|
  rd.main = "README2.rdoc"
  rd.rdoc_files.include 'README*', 'docs/*', 'LICENSES.txt'
  rd.rdoc_files.include 'src/kernel/bootstrap/Maglev.rb'
  rd.rdoc_files.include 'examples/**/README*', 'examples/**/readme.txt'
  rd.rdoc_files.include 'src/topaz/**/README*', 'src/topaz/LICENSE*'
end

task :default => :status

desc "Show status of all stones"
task :status do
  sh "$GEMSTONE/bin/gslist -clv" do |ok, status|
    case status.exitstatus
    when 0, 1
      # Ok, or no servers running: do nothing
    else
      raise "gslist failed: #{status.exitstatus}"
    end
  end
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
task :squeak => 'netldi:start' do
  os = `uname`.chomp
  if os == "Darwin"
    gem_tools = '/Applications/GemTools-MagLev.app'
    if File.exists?(gem_tools)
      sh %{ open #{gem_tools} }
    else
      puts "Cannot open #{gem_tools}"
      puts "as that file does not exist. To fix this, download and unzip"
      puts "http://seaside.gemstone.com/squeak/GemTools-MagLev.zip"
      puts "then move GemTools-MagLev.app to /Applications"
    end
  else
    gem_tools = "#{MAGLEV_HOME}/../GemTools-MagLev.app/GemTools.sh"
    if File.exists?(gem_tools)
      sh "#{gem_tools}"
    else
      puts "Cannot open #{gem_tools}"
      puts "as that file does not exist. To fix this, download and unzip"
      puts "http://seaside.gemstone.com/squeak/GemTools-MagLev.zip"
      puts "then move GemTools-MagLev.app to \$MAGLEV_HOME/../"
    end
  end
end

desc "Create .rb files for each smalltalk class (lib/ruby/site_ruby/1.8/smalltalk/*)"
task :stwrappers, :force  do |t, args|
  wrapper_dir = MAGLEV_HOME + '/lib/ruby/site_ruby/1.8/smalltalk'
  if ! File.exist?(wrapper_dir) || args.force
    puts "Creating .rb files for smalltalk classes in lib/ruby/site_ruby/1.8/smalltalk/"
    run_on_stone(["omit resultcheck",
                  "run",
                  "RubyContext createSmalltalkFFIWrappers",
                  "%"])
  else
    puts "#{wrapper_dir} already exists"
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
    puts "Creating server \"#{args.server_name}\""
    MagLevStone.create(args.server_name)
  end

  desc "Destroy an existing MagLev server and repository"
  task :destroy, :server_name do |t, args|
    raise ArgumentError, "Task #{t.name} requires an existing server name" unless args.server_name
    s = MagLevStone.existing(args.server_name)
    s.stop
    puts "Destroying server \"#{args.server_name}\""
    s.destroy!
  end

  desc "Invoke a task on all MagLev servers"
  task :all, :task_name do |t,args|
    GemStoneInstallation.current.stones.each do |server_name|
      Rake::Task["#{server_name}:#{args.task_name}"].invoke
    end
  end
end

# Run topaz commands on a particular stone
def run_on_stone(commands_array, stone=(ENV['STONENAME'] || 'maglev'))
  Stone.new(stone).topaz_commands(commands_array)
end

namespace :netldi do
  desc "Stop NetLDI process"
  task :stop do
    GemStoneInstallation.current.stopnetldi
  end
  desc "Start NetLDI process"
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

    desc "Read a GemStone Topaz .gs file into server.  Does a commit."
    task :input_file, :file do |t, args|
      file = args[:file]
      raise "Need a file to read." unless file
      raise "Can't open input file: #{file.inspect}" unless File.exists?(file)
      stone.input_file file, true
    end
  end
end
