# These applications seem to be easily broken, so we require the code to
# make sure we can at least load them, if not run them at every checkin.

ext_dir = File.dirname(__FILE__) + '/../external/'

$: << ext_dir + 'rack-1.0.0/lib'
$: << ext_dir + 'sinatra-0.9.3/lib'

require 'webrick'
require 'rack'
#require 'sinatra'
