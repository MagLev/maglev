$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Sinatra/lib')
$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Rack/lib')

require 'sinatra'

#set_trace_func proc {  |event, file, line, id, binding, classname| printf "%8s %s:%-2d %10s %8s\n", event, file, line, id, classname }
get '/hi' do
  "Hello world"
end
