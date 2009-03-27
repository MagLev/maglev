$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Sinatra/lib')
$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Rack/lib')

require 'sinatra'

configure(:development) do
#  set :sessions, true  # Can't do this, as no openssl support
#  set :logging, false
#  set :reload,  false
  set :run,     true
  set :server,  'webrick'
  set :lock,    false     # but I'm only sending one request at a time....
end

# These examples are from the Sinatra Book
# http://www.sinatrarb.com/book.html

get '/' do
  "Hello World"
end

get '/names/:name' do
  "The name is: #{params[:name]}"
end

get '/say/*/to/*' do
  splats = params['splat']
  "Say '#{splats[0]}' to #{splats[1]}"
end

# get '/noname/agent', :agent => /Songbird/ do
#   "Using Songbird"
# end
# get '/noname/agent' do
#   "NOT Using Songbird  agent: #{params[:agent]}"
# end

get '/goto_home' do
  redirect '/'
end

# Sessions depend on cookies which depend on openssl which is not supported
# due to c-extensions
#
# get '/session_count' do
#   session["counter"] ||= 0
#   session["counter"] += 1
#   "count: #{session['counter']}"
# end

get '/not_found' do
  status 404
  "Custom Not Found"
end

get '/throw_not_found' do
  throw :halt, [404, 'Thrown Not Found']
end
