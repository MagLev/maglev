# These applications seem to be easily broken, so we require the code to
# make sure we can at least load them, if not run them at every checkin.

ext_dir = File.dirname(__FILE__) + '/../external/'

$: << ext_dir + 'rack/lib'
$: << ext_dir + 'sinatra/lib'

require 'webrick'

require 'rack'
# Rack autoloads everything, so you have to mention a constant in the
# source to force loading of it.  We don't do complete coverage, but we try
# to load a few things.
Rack::Handler::WEBrick
Rack::CommonLogger
Rack::File
Rack::Lint
Rack::ShowStatus
Rack::ShowExceptions
Rack::Static
Rack::Request
Rack::Response

#require 'sinatra'
