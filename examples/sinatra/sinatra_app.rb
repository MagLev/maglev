require 'rubygems'
require 'sinatra'

# Workaround for issue automatically starting app.
# See comments in README file
set :run,     true
set :server,  'webrick'

get '/' do
  "Sinatra #{Sinatra::VERSION} says Hello"
end
