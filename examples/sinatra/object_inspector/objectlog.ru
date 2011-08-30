require 'sinatra'
require 'objectlog_app'

set :server, 'webrick'
set :environment, :development
set :static, true

# ObjectLogApp is relocatable, so tell it where it will start
ObjectLogApp.script_name  = ''
ObjectLogApp.main_app_url = '/'
ObjectLogApp.main_object  =  ObjectLogEntry.object_log

ObjectLogApp.run! :host => 'localhost', :port => 4567
