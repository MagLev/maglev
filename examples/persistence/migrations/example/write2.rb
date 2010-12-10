# This script writes a bunch of posts and stores them in the system.
# Uses new pub date feature to random effect.

now_secs = Time.now.to_i  # number of seconds since epoch
5.times do |i|
  pub_date = Time.at(rand(now_secs))
  BlogPost.all_posts << BlogPost.new("Title #{i}", "Text #{i}", pub_date)
end
Maglev.commit_transaction
