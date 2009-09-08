
force = ARGV[0] =~ /force/

Maglev.abort_transaction
if force or not defined? Post
  Maglev.persistent do
    load 'blog.rb'
    load 'txn_wrapper.rb'
    load 'maglev/objectlog.rb'
  end
  Maglev.commit_transaction
  puts "== Committed blog.rb"
else
  puts "== blog.rb is already committed....skipping"
end
