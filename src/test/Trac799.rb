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
