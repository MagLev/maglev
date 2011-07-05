# Test that the lock API methods are available.

# Passes if no exceptions raised

o = Object.new

Maglev.begin_transaction

Maglev::System.read_lock o
Maglev::System.remove_lock o

Maglev::System.write_lock o

Maglev.commit_and_release_locks

Maglev.begin_transaction
Maglev::System.write_lock o
Maglev::System.remove_locks_for_session
