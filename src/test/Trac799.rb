require 'socket'

sa = TCPServer.new(5555)
STDIN.reopen(sa)
sb = STDIN

sc = Socket.for_fd(STDIN.fileno)   # coverage for Trac 805
unless sb._equal?(sc) ; raise 'Fail A'; end
unless sb.class._equal?(TCPServer) ; raise 'Fail B'; end

# Be sure to close, other tests use this port....
sa.close
sc.close
puts "OK"
true
#################### Trac Info
# ID:         799
# Summary:    IO.reopen only works with File objects, not other IO objects
# Changetime: 2010-10-14 19:10:32+00:00
###

#  From Rack::Handler::FastCGI
#  
#  
#  {{{
#  require 'socket'
#  STDIN.reopen(TCPServer.new(5555))
#  }}}
#  
#  MRI handles ok, but MagLev:
#  
#  
#  {{{
#  #<TypeError: File#reopen, first arg must be a File or a String>
#  /Users/pmclain/GemStone/dev/pbm.rb:3:in `raise'
#  /Users/pmclain/GemStone/dev/pbm.rb:3:in `reopen'
#  /Users/pmclain/GemStone/dev/pbm.rb:3
#  ERROR 2023, Error, 'File#reopen, first arg must be a File or a String' (TypeError)
#  }}}
#  
#  