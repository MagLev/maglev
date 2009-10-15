# Execute with:
# rake -f example_glass_rakefile.rb -T

require 'glass_stone'

# This is an example Rakefile for a GLASS installation.
# For your own installation, extend Stone/GlassStone and override to
# change the behaviour for your situation.
#
# Your initialize_new_stone will most probably differ, for example.
# 
# Set to true to see what commands gets executed
verbose(false)

task :default do
  puts "Example tasks for managing a GemStone installation"
end

desc "Create a new stone"
task :new_stone, :stone_name do |t, args|
  puts "Creating #{args.stone_name}"
  GlassStone.create(args.stone_name)
end

desc "Server status"
task :status do
  GemStoneInstallation.current.status
end

desc "Stop netldi"
task :stopnetldi do
  GemStoneInstallation.current.stopnetldi
end

desc "Start netldi"
task :startnetldi do
  GemStoneInstallation.current.startnetldi
end

def task_gemstone(stone, action)
    desc "#{action.to_s} - #{stone.name}"
    task action do
      stone.send(action)
    end
end

GemStoneInstallation.current.stones.each do |stoneName|
  namespace stoneName do
    stone = GlassStone.new(stoneName)

    [:stop, :start, :restart, :status, :backup, :restore_latest_backup].each do |action|
      task_gemstone(stone, action)
    end
  end
end
