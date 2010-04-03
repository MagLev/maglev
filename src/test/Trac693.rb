# MagLev was generating the following error:
#
#
#  $ maglev-ruby $xxx
#  #<RuntimeError: The object LibPsych may not be committed, 'it is a transient Ruby module/class'>
#  /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb:11:in `commit_transaction'
#  /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb:11:in `commit_transaction'
#  /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb:11
#  ERROR 2407, The object LibPsych may not be committed, 'it is a transient Ruby module/class' (RuntimeError)
require 'yaml'
Maglev.commit_transaction
