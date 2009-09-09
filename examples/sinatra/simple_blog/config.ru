# This runs both the blog app and the object log viewer as separate
# Sinatra apps in the same process

require 'sinatra'

require 'blog_app'
require 'objectlog_app'

disable :run
set :environment, :development

map "/" do
  run BlogApp
end

map "/objectlog" do
  ObjectLogApp.path = "/objectlog"
  run ObjectLogApp
end




