require 'sinatra'
require 'objectlog_app'

set :server, 'webrick'
set :environment, :development
set :static, true

ObjectLogApp.run! :host => 'localhost', :port => 4567
