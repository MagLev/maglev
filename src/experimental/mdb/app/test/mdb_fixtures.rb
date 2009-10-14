# These are fixtures that need to be loaded into MagLev before the
# mdb_tests.rb can be run.
Maglev.persistent do
  load 'app_model.rb'
end
Maglev.commit_transaction
puts "Maglev committed #{AppModel}"

server = MDB::Server.server
# Create test db on server
DB_NAME = 'rest_database_tests'  # See also mdb_tests.rb
if server.key? DB_NAME
  server.delete DB_NAME
end

server.create(DB_NAME, AppModel)
p AppModel.view_42
