# This file is passed to MagLev via the Rakefile and used to load the
# various databases.

Maglev.persistent do
  load 'lib/blog.rb'  # Do not use require
end
Maglev.commit_transaction

server = MDB::Server.server
# MDB::Server etc. manages its own transactions
[['theBlogPosts', Post],
 ['theBlogTags',   Tag]
].each do |key, view|
  if server.key? key
    puts "Server has db named: #{key}...updating with #{view}"
    server.update(key, view)
  else
    puts "Server does not have db named: #{key}...creating with #{view}"
    server.create(key, view)
  end
end
