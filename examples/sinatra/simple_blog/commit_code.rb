# This file loads the class definitions for the persistable classes in the
# blog app and commits them.  If first tests to see if the code is already
# committed, and will only re-commit if "force" is passed as the first
# parameter.
force = ARGV[0] =~ /force/

Maglev.abort_transaction
if force or not defined? Post
  Maglev.persistent do
    # Use load rather than require to force re-reading of the files
    load 'blog.rb'
    load 'txn_wrapper.rb'
    load 'maglev/objectlog.rb'
  end
  Maglev.commit_transaction
  puts "== Committed blog.rb"
else
  puts "== blog.rb is already committed....skipping"
end
