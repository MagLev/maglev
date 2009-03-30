# Since RubyGems isn't working, and we need some rackup help, this
# emulates loading the rack gem and kicking off rackup

sinatra_dir = File.dirname(__FILE__) + '/../../src/external/Sinatra/lib'
rack_dir    = File.dirname(__FILE__) + '/../../src/external/Rack/lib'
rackup      = File.dirname(__FILE__) + '/../../src/external/Rack/bin/rackup'
$:.unshift(sinatra_dir)
$:.unshift(rack_dir)

options = { :Port => 4567, :Host => "0.0.0.0" }
require 'rack'
config = 'config.ru'
cfgfile = File.read(config)
inner_app = eval "Rack::Builder.new {( " + cfgfile + "\n )}.to_app", TOPLEVEL_BINDING, config
server = Rack::Handler::WEBrick
app = Rack::Builder.new {
  use Rack::CommonLogger, STDERR  unless server.name =~ /CGI/
  use Rack::ShowExceptions
#  use Rack::Lint
  run inner_app
}.to_app

server.run app, options
