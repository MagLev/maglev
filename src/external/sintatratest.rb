$:.unshift (ENV[:MAGLEV_HOME] + '/src/external/Sinatra/lib')
$:.unshift (ENV[:MAGLEV_HOME] + '/src/external/rack-0.9.0/lib')

require 'sinatra'
