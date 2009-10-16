# Clean out a previous test DB if it exists
key = 'PERSISTENT_DB_TEST'
server = MDB::Server.server
if server.key?(key)
  puts "Deleting old database #{key}"
  server.delete key
end


# Create two persistent view classes
Maglev.abort_transaction
Maglev.persistent do
  class PView
    def view
      1
    end
  end
  class PView2
    def view
      2
    end
  end
end
Maglev.commit_transaction

# Create the DB
db = server.create(key, PView)

# Add some documents
4.times { db.add(Object.new) }
Maglev.commit_transaction


