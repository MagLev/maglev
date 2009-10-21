# This migration adds some test data for version 000
db = MDB::Server.server['theBlogPosts']
SECS_PER_DAY = 60 * 60 * 24
10.times do |i|
  p = Post.new(:title => "Title #{i}",
               :text => "text #{i}",
               :timestamp = Time.now - (i*SECS_PER_DAY))
  db.add(p)
end
Maglev.commit_transaction

