# This file loads the class definitions for the persistable classes that
# implement the MDB Server and commits them to the MagLev repository.

# Itfirst tests to see if the code is already committed, and will only
# re-commit if "force" is passed as the first parameter.
force = ARGV[0] =~ /force/

Maglev.abort_transaction
#if force or not defined? ::MDB
  Maglev.persistent do
    # Use load rather than require to force re-reading of the files
    load 'lib/mdb/common.rb'
    load 'lib/mdb/server.rb'
    load 'lib/mdb/database.rb'
  end
  Maglev.commit_transaction
  puts "== Committed MDB Server code"
#else
#  puts "== MDB Server code is already committed....skipping"
#end
