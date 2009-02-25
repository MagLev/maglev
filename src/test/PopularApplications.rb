# These applications seem to be easily broken, so we require the code to
# make sure we can at least load them, if not run them at every checkin.

ext_dir = File.dirname(__FILE__) + '/../external/'

$: << ext_dir + 'Rack/lib'
$: << ext_dir + 'Sinatra/lib'

require 'webrick'
require 'rack'
#require 'sinatra'
