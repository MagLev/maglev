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

# px = Psych::LibPsych   # debugging code
# cls = class << px
#  self
# end
# nil.pause
# Maglev::System.trap_add_to_closure_list( cls )

Maglev.commit_transaction
true
#################### Trac Info
# ID:         745
# Summary:    require of an FFI library raised errors at commit -  get rid of 'moduleRef' objects from AST
# Changetime: 2011-03-03 17:47:23+00:00
###

#  The code:
#  {{{
#  require 'psych'
#  Maglev.commit_transaction
#  }}}
#  
#  The Error:
#  
#  {{{
#  #<RuntimeError: The object LibPsych may not be committed, 'it is a transient Ruby module/class'>
#  /Users/pmclain/GemStone/dev/pbm.rb:19:in `commit_transaction'
#  /Users/pmclain/GemStone/dev/pbm.rb:19:in `commit_transaction'
#  /Users/pmclain/GemStone/dev/pbm.rb:19
#  ERROR 2407, The object LibPsych may not be committed, 'it is a transient Ruby module/class' (RuntimeError)
#  }}}
#  
#  It seems to be any of the FFI libs, as the following also fails;
#  
#  {{{
#  require 'maglev/openssl/ffi/libcrypto.rb'
#  Maglev.commit_transaction
#  }}}
#  
#  