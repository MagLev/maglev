require 'rubygems'
require 'sinatra'
require 'haml'

set :run, true

get '/' do
  @data = "Hi there"
  haml :index
end
