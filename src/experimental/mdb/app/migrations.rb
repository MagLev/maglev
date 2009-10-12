# This file is passed to MagLev via the Rakefile and used to load the
# various databases.

Maglev.persistent do
  load 'lib/blog.rb'  # Do not use require
end
Maglev.commit_transaction

# MDB::Server etc. manages its own transactions
[['theBlogPosts', Post],
 ['theBlogTags',   Tag]
].each do |key, view|
  if MDB::Server.key? key
    puts "Server has db named: #{key}...updating with #{view}"
    MDB::Server.update(key, view)
  else
    puts "Server does not have db named: #{key}...creating with #{view}"
    MDB::Server.create(key, view)
  end
end
