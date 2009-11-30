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
sinatra_dir = "#{gem_dir}/sinatra-0.9.4/lib"
rack_dir    = "#{gem_dir}/rack-1.0.1/lib"

[sinatra_dir, rack_dir].each do |dir|
  raise "Could not find #{dir}" unless File.directory? dir
  $:.unshift dir
end

Maglev.persistent { require 'sinatra' }
Maglev.commit_transaction
