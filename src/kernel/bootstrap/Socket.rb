#   We currently do not have a BasicSocket class .
#   Ruby BasicSocket and Socket methods are combined into Smalltalk Socket .
#
#   TCPSocket.class open()  does a blocking connect attempt
#   and returns a blocking socket.
#
#   successful TCPServer.accept  returns a new socket whose blocking
#   state is the same as the receiver's
#
#   All other newly created sockets default to non-blocking. Use 
#      aSocket.setsockopt('SOL_TCP', 'SO_NONBLOCKING', false)  
#   to change a socket to blocking.
#
#   The above is based on the current plan to use blocking sockets
#   for the demo and use methods defined in the existing Ruby socket API.

class Socket
  # accept implemented only in TCPServer .
  # bind, listen not implemented,   
  #   use  TCPServer>>new:port:  to create a listening socket

  # send, write, recv  function per the non-blocking state of the receiver.
  primitive 'send', 'send:flags:'
  primitive 'send', 'send:flags:addr:'
  primitive 'write', 'write:'
  primitive '<<', 'write:'
  primitive 'recv', 'recv:' 
  primitive 'read', 'recv:' 

  # Note, assignment of result to Ruby global $_  not implemented yet.
  primitive 'gets', 'gets'
  primitive 'gets', 'gets:'

  primitive 'close', 'close'
  primitive 'shutdown', 'shutdown'
  primitive 'shutdown', 'shutdown:' 
  primitive 'connected?', 'isConnected'

  def eof?
    conn = connected?
    !conn
  end
  
  # following 2 are in BasicSocket in Ruby 1.8 , but put in Socket for now,
  #  and they only support access to SO_NONBLOCKING , examples:
  #    aSocket.getsockopt('SOL_TCP', 'SO_NONBLOCKING')
  #    aSocket.setsockopt('SOL_TCP', 'SO_NONBLOCKING', aBoolean)
  primitive 'setsockopt', 'setsockopt:name:value:'
  primitive 'getsockopt', 'getsockopt:name:'

  self.class.primitive 'do_not_reverse_lookup', 'setNoReverseLookup:'
# do we have a problem here ?
#  self.class.primitive 'getaddrinfo', 'getaddrinfo:port:family:type:protocol:flag:' 
  self.class.primitive '_getaddrinfo', 'getaddrinfo:port:' 
  self.class.primitive 'gethostbyname', 'gethostbyname:'
  self.class.primitive 'gethostname', 'getLocalHostName'
  self.class.primitive 'new', 'new:type:proto:'
  
# problem ?
  def self.getaddrinfo(name, port, *args)
    [[2, port, "localhost", "127.0.0.1", nil, nil]]
  end
end

class IPSocket
  self.class.primitive 'peeraddr', 'peeraddr'
  self.class.primitive 'getaddress', 'getHostAddressByName:'
end

class TCPSocket

  # open binds a socket to a port and does a blocking connect, 
  #  returning a blocking socket.
  self.class.primitive 'open', 'new:port:'
end

class TCPServer
  # accept returns a new socket, having same non-blocking state as receiver
  primitive 'accept', 'accept'

  # new and open  both create a non-blocking listening socket
  self.class.primitive 'new', 'new:port:'
  self.class.primitive 'open', 'new:port:'
end






