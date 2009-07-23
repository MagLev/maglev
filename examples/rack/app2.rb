$:.unshift "#{ENV['MAGLEV_HOME']}/src/external/rack/lib"

require 'webrick'
require 'rack'

def app_method(env)
  [200, { "Content-Type" => "text/html"}, "Hello from #{__FILE__}"]
end
Rack::Handler::WEBrick.run method(:app_method), :Port => 9292
