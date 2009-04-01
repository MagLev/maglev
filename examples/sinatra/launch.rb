# Since RubyGems isn't working, and we need some rackup help, this
# emulates loading the rack gem and kicking off rackup

puts "== launch.rb"
require 'setup.rb'

options = { :Port => 4567, :Host => "0.0.0.0" }
require 'rack'
config = File.dirname(__FILE__) + '/config.ru'
cfgfile = File.read(config)
puts "============================="
inner_app = eval "Rack::Builder.new {( " + cfgfile + "\n )}.to_app", nil, config
puts "============================="
server = Rack::Handler::WEBrick
app = Rack::Builder.new {
  use Rack::CommonLogger, STDERR  unless server.name =~ /CGI/
#  use Rack::ShowExceptions
#  use Rack::Lint
  run inner_app
}.to_app

puts "== launch.rb: calling server.run"
server.run app, options
