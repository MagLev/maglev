# This runs both the blog app and the object log viewer as separate
# Sinatra apps in the same process
#
#\ --port 4444 --host localhost --server 'webrick'

require 'sinatra'
require 'blog_app'
require '../object_inspector/objectlog_app'

map "/" do
  run BlogApp
end

map "/objectlog" do
  # Tell the ObjectLogApp where it is mounted.
  ObjectLogApp.script_name  = "/objectlog"
  ObjectLogApp.main_app_url = "http://localhost:4444/"  # A somewhat informed guess
  ObjectLogApp.main_object  = "Maglev::PERSISTENT_ROOT[SimplePost]"
  run ObjectLogApp
end
