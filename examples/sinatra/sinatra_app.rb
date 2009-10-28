require 'rubygems'
require 'sinatra'

# Workaround for issue automatically starting app.
# See comments in README file
if defined? Maglev and not defined? DO_NOT_RUN
  set :run,     true
  set :server,  'webrick'
end

get '/' do
  "Sinatra #{Sinatra::VERSION} says Hello"
end
