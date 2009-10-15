require 'lib/mdb_server'
require '../../../examples/sinatra/object_inspector/objectlog_app'

disable :run
set :environment, :development
set :server, ["webrick"]   # Maglev currently only supports webrick

options = { :Port => 4567, :host => 'localhost' }

map "/" do
  run MDB::ServerApp
end

map "/objectlog" do
  # Tell the ObjectLogApp where it is mounted and what the main app is
  ObjectLogApp.script_name  = "/objectlog"
  ObjectLogApp.main_app_url = "http://localhost:3333/"  # A somewhat informed guess
  ObjectLogApp.main_object  = "Maglev::PERSISTENT_ROOT[MDB::Server]"
  run ObjectLogApp
end





