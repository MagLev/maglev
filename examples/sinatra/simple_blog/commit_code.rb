Maglev.abort_transaction
Maglev.persistent do
  load 'blog.rb'
end
Maglev.commit_transaction
puts "== Committed blog.rb"
