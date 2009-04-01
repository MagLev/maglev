puts "== config.ru"
require 'setup.rb'
require 'sinatra'

here = File.dirname(__FILE__)
disable :run              # Prevent Sinatra from running out of at_exit handler
set :views,  here + '/views'
set :public, here + '/public'
set :app_file, __FILE__

# log = File.new('./sinatra_log', 'a')
# STDOUT.reopen(log)
# STDERR.reopen(log)

require 'sinatra_blog.rb'
run Sinatra::Application
