# This file is passed to MagLev via the Rakefile and used to load the
# various databases.

Maglev.persistent do
  load 'lib/blog.rb'  # Do not use require
end
Maglev.commit_transaction

# MDB::Server etc. manages its own transactions
key = 'post'
if MDB::Server.key? key
  puts "Server has db named: #{key}...updating"
  MDB::Server.update(key, Post)
else
  puts "Server does not have db named: #{key}...creating"
  MDB::Server.create(key, Post)
end
