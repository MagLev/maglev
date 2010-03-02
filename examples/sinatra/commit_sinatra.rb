# A script to commit Sinatra to MagLev.
#
# NOTE: If you do this, then Sinatra will not start from the at_exit
#       handler, since the Sinatra code has already been loaded (i.e., the
#       at_exit handler has already been run).
#
# Currently, there are some problems committing the RubyGems framework into
# MagLev, so we load specific versions of Rack and Sinatra into MagLev by
# hand.  If you do not have these versions, then edit this file to reflect
# the versions you have.

gem_dir     = "#{ENV['MAGLEV_HOME']}/lib/maglev/gems/1.8.6/gems"
Dir.glob("#{gem_dir}/{rack-[0-9]*,sinatra-[0-9]*}").each do |dir|
  raise "Could not find #{dir}" unless File.directory? dir
  $:.unshift dir
end

Maglev.persistent { require 'sinatra' }
Maglev.commit_transaction
