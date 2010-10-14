# Problem 1:
#   Socket.listen() is undefined.  We have listen on tcp socket only, but
#   you can open a plain vanilla socket with SOCK_STREAM and then listen is
#   ok.

require 'socket'

socket = Socket.new( Socket::AF_INET, Socket::SOCK_STREAM, 0 )
socket.listen( 1 )

# Problem 2:
#
# The call to listen raises a SocketError: NotConnected
p server = TCPServer.new('5555')
p server.listen(100)
true
