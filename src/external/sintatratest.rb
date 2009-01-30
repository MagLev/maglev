$:.unshift (ENV[:MAGLEV_HOME] + '/src/external/Sinatra/lib')
$:.unshift (ENV[:MAGLEV_HOME] + '/src/external/Rack/lib')

require 'sinatra'
