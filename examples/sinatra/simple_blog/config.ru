# This runs both the blog app and the object log viewer as separate
# Sinatra apps in the same process

require 'sinatra'

require 'blog_app'
require 'objectlog_app'

disable :run
set :environment, :development

options = { :Port => 4567, :host => 'localhost' }

map "/" do
  run BlogApp
end

map "/objectlog" do
  # Tell the ObjectLogApp where it is mounted.
  ObjectLogApp.script_name = "/objectlog"
  run ObjectLogApp
end


