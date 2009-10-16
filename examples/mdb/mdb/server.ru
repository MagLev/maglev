require 'lib/mdb_server'
require '../../examples/sinatra/object_inspector/objectlog_app'

MDB::ServerApp.run! :host => 'localhost',
                    :port => 4567,
                    :server => 'webrick'
