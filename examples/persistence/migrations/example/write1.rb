# This script writes a bunch of posts and stores them in the system.

5.times do |i|
  BlogPost.all_posts << BlogPost.new("Title #{i}", "Text #{i}")
end
Maglev.commit_transaction
