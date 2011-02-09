# Normal sinatra apps run out of the at_exit handler.
# MagLev backtrace format confuses sinatra, so the web app doesn't start.
# There are several work-arounds.
#   set :app_file, __FILE__
# or
#   set :run, true
#
# This file is here to manually test against trac 605: Does Maglev start
# the server or not?

require 'rubygems'
require 'sinatra'

get '/hi' do
  "Howdy"
end


