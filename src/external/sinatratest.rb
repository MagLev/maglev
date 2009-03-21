$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Sinatra/lib')
$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Rack/lib')

require 'sinatra'

configure(:development) do
  set :logging, false
  set :reload,  false
  set :run,     true
  set :server,  'webrick'
  set :lock,    false     # but I'm only sending one request at a time....
end

get '/' do
  "Hello world"
end
