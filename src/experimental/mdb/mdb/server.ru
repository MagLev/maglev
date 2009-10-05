require 'lib/mdb_server'
require 'log_headers'

MDB::ServerApp.run! :host => 'localhost',
                    :port => 4567,
                    :server => 'webrick'

