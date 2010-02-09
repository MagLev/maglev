# MagLev was returning:
#
# error , SocketError_unknown  port name not found,
#           during /Users/pmclain/GemStone/checkouts/git/src/test/TracXXX.rb
# ERROR 2023, Error, 'SocketError_unknown  port name not found' (SocketError)

require 'socket'

address = "0.0.0.0"   # Equivalent to INADDR_ANY
port = "4567"

res = Socket::getaddrinfo(address, port,
                          Socket::AF_UNSPEC,   # address family
                          Socket::SOCK_STREAM, # socket type
                          0,                   # protocol
                          Socket::AI_PASSIVE)  # flag
