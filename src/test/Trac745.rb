# This test passes if no exceptions are raised.
# A simple require of an FFI based library caused commit problems.
#
#   $ maglev-ruby src/test/Trac745.rb
#   #<RuntimeError: The object LibPsych may not be committed, 'it is a transient Ruby module/class'>
#   /Users/pmclain/GemStone/checkouts/git/src/test/Trac745.rb:12:in `commit_transaction'
#   /Users/pmclain/GemStone/checkouts/git/src/test/Trac745.rb:12:in `commit_transaction'
#   /Users/pmclain/GemStone/checkouts/git/src/test/Trac745.rb:12
#   ERROR 2407, The object LibPsych may not be committed, 'it is a transient Ruby module/class' (RuntimeError)

require 'psych'
Maglev.commit_transaction
