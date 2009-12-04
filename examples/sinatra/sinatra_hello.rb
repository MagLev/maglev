require 'rubygems'
require 'sinatra'

configure(:development) do
  set :sessions, true
#  set :logging, false
#  set :reload,  false
  set :run,     true
  set :server,  'webrick'
  set :lock,    false     # but I'm only sending one request at a time....
end

# These examples are from the Sinatra Book
# http://www.sinatrarb.com/book.html

get '/' do
  <<-EOS
  <html>
    <head><title>Sinatra Hello</title></head>
    <body>
      <h2>Hello from Sinatra</h2>
      <p>Rack: #{Rack.release}</p>
      <p>Sinatra: #{Sinatra::VERSION}</p>
      <h2>Some test URLs</h2>
      <ul>
        <li><a href="/names/fred">/names/fred</a></li>
        <li><a href="/say/hi/to/Sinatra">/say/hi/to/Sinatra</a></li>
        <li><a href="/goto_home">/goto_home</a></li>
        <li><a href="/session_count">/session_count</a></li>
        <li><a href="/not_found">/not_found</a></li>
        <li><a href="/throw_not_found">/throw_not_found</a></li>
      </ul>
    </body>
  </html>
  EOS
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

get '/session_count' do
  session["counter"] ||= 0
  session["counter"] += 1
  "count: #{session['counter']}"
end

get '/not_found' do
  status 404
  "Custom Not Found"
end

get '/throw_not_found' do
  throw :halt, [404, 'Thrown Not Found']
end
