# Create a prefix for databases, so we can remove all test DBs w/o
# messing with other DBs served by the same MagLev repository
module MDB::Test
  TEST_DB_PREFIX = "minitest_db"
  TEST_DB_REGEXP = /^#{MDB::Test::TEST_DB_PREFIX}/

  module_function

  # Delete from Server, all databases whose name starts with "minitest_db".
  def delete_test_dbs
    MDB::Server.db_names.each do |name|
      if name =~ MDB::Test::TEST_DB_REGEXP
        begin
          MDB::Server.delete name
        rescue MDB::Server::DatabaseNotFound
          $stderr.puts "--- unexpected #{e} in before()"
        end
      end
    end
  end

  def db_name(suffix)
    key = MDB::Test::TEST_DB_PREFIX + suffix
  end

end

#unless defined? ViewClass and ViewClass.maglev_persistable?
STDERR.puts "== Committing the ViewClass"
Maglev.abort_transaction
Maglev.persistent do
  class ViewClass
    def self.view_42
      42
    end
  end
end
Maglev.commit_transaction
#end
