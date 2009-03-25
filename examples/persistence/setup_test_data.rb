# This file manages the db...  We'll define some persistent roots, but this
# file is not included/loaded by the other files, so Sinatra won't cause
# undue problems with our data.
#
# MagLev doesn't yet run Rake....
require 'maglev'

def preload_code(*files)
  # Load and commit the code we'll use for the blog application.
  # We use load and not require to force reading the latest files.
  MagLev::CodeAndData.begin_txn
  files.each { |f| load f }
  MagLev::CodeAndData.commit_txn
end

def preload_app_code
  preload_code('maglev.rb', 'blog.rb', 'post.rb')
end

def load_fixtures
  # Create a new blog in $my_blog and seed with test data.

  # Note: can't do a begin_txn, as that blows away the require of
  # *this* file as well as any other files required...
  $my_blog ||= Blog.new("My Blog")
  10.times do |i|
    a_post = Post.new("Post-#{i}", "This is the body for post #{i}")
    $my_blog.add_post(a_post)
  end
  MagLev::CodeAndData.commit_txn
end

def preload_sinatra
  MagLev::CodeAndData.begin_txn
  rack_dir    = File.dirname(__FILE__) + '/../../src/external/Rack/lib'
  sinatra_dir = File.dirname(__FILE__) + '/../../src/external/Sinatra/lib'
  $:.unshift(sinatra_dir)
  $:.unshift(rack_dir)
  require 'maglev'
  load "#{sinatra_dir}/sinatra.rb"
  MagLev::CodeAndData.commit_txn

  # This configure prevents sinatra from trying to run after the require.
  # Sinatra starts in the at_exit handler.  But do this outside of the
  # transaction, so we don't permanently set the flag.
  configure(:development) { set :run, false }
end

#preload_app_code
#load_fixtures
preload_sinatra
