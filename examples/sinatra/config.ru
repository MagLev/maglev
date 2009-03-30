HERE = File.dirname(__FILE__)
SINATRA_DIR = HERE + '/../../src/external/Sinatra/lib'
RACK_DIR    = HERE + '/../../src/external/Rack/lib'

$:.unshift(SINATRA_DIR)
$:.unshift(RACK_DIR)
$:.unshift(File.dirname(__FILE__))

require 'sinatra'

disable :run              # Prevent Sinatra from running out of at_exit handler
set :views,  HERE + '/views'
set :public, HERE + '/public'
set :app_file, __FILE__

# log = File.new('./sinatra_log', 'a')
# STDOUT.reopen(log)
# STDERR.reopen(log)

require 'simple_sinatra.rb'
run Sinatra::Application
