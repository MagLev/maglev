# These are fixtures that need to be loaded into MagLev before the
# mdb_tests.rb can be run.
Maglev.persistent do
  load 'app_model.rb'
end
Maglev.commit_transaction
puts "Maglev committed #{AppModel}"

# Create test db on server
DB_NAME = 'rest_database_tests'  # See also mdb_tests.rb
if MDB::Server.key? DB_NAME
  MDB::Server.delete DB_NAME
end

MDB::Server.create(DB_NAME, AppModel)
p AppModel.view_42
