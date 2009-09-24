require 'lib/mdb_server'

MDB::ServerApp.run! :host => 'localhost',
                    :port => 4567,
                    :server => 'webrick'
