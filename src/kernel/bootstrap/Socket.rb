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
#      aSocket.set_blocking(false)
#   to change a socket to blocking.
#
#   The above is based on the current plan to use blocking sockets
#   for the demo and use methods defined in the existing Ruby socket API.

class Socket

  # OS dependent constants initialized by _init_socket_constants
  # in second opening of Socket, below

  # accept implemented only in TCPServer .
  # bind, listen not implemented,
  #   use  TCPServer>>new:port:  to create a listening socket

  # send, write, recv  function per the non-blocking state of the receiver.
  primitive_nobridge 'send', 'send:flags:addr:'
  primitive 'send', 'send:flags:'
  primitive_nobridge '<<', 'write:'
  primitive 'write', 'write:'
  primitive 'recv', 'recv:'
  primitive 'read', 'recv:'

  def sysread(num_bytes)
    s = self.read(num_bytes)
    raise EOFError if s.nil?
    return s
  end

  def flush
    # nothing to do, no buffering in the VM for socket write operations
    self
  end

  def gets(*args, &blk)
    raise ArgumentError, 'expected 0 or 1 arg'
    #  variants other than gets , gets(terminator) not supported
    # because bridge methods would interfere with use of _storeRubyVcGlobal
  end
  # Smalltalk implementations of   gets  responsible for
  #    _storeRubyVcGlobal  to  store into caller's $_ if any
  primitive_nobridge 'gets', 'gets'
  primitive_nobridge 'gets', 'gets:'

  primitive 'close', 'close'
  primitive '_active?', 'isActive'
  primitive_nobridge 'shutdown', 'shutdown'
  primitive 'shutdown', 'shutdown:'
  primitive 'connected?', 'isConnected'

  def closed?
    ! _active?
  end

  def eof?
    conn = connected?
    !conn
  end

  # following 2 are in BasicSocket in Ruby 1.8 , but put in Socket for now,

  primitive_nobridge '_setsockopt', 'setsockopt:name:value:'

  primitive '_getsockopt', 'getsockopt:name:'

  def getsockopt(level, optname)
    #  result is a Fixnum except for
    #  optname's SO_RCVTIMEO, SO_SNDTIMEO, SO_LINGER which return a 2 element Array
    arr = _getsockopt(level, optname)
    if arr._isFixnum
      Errno.raise(arr)
      res = nil
    elsif arr.size.equal?(1)
      res = arr[0]  # a Fixnum
    else
      # 2 element Array from SO_RCVTIMEO, SO_SNDTIMEO, or SO_LINGER
      res = arr
    end
    res
  end

  def setsockopt(level, optname, optval)
    # optval should be a Fixnum or true or false except for
    # optname's SO_RCVTIMEO, SO_SNDTIMEO, SO_LINGER which require a 2 element Array
    #   of Fixnums .
    if optval.equal?(true)
      optval = 1
    elsif optval.equal?(false)
      optval = 0
    end
    status = _setsockopt(level, optname, optval)
    if status.equal?(0)
      return self
    else
      Errno.raise(status)
    end
  end

  # def set_blocking(a_boolean)  ; end
  #  this is a  workaround until fcntl() is implemented in IO.rb
  primitive 'set_blocking', 'setBlocking:'

  class_primitive 'do_not_reverse_lookup=', 'setNoReverseLookup:'
  class_primitive_nobridge '_getaddrinfo', '_getaddrinfo:'  # one arg , an Array of 6 elements
  class_primitive 'gethostbyname', 'gethostbyname:'
  class_primitive '_getservbyname', 'getservbyname:protocol:'
  class_primitive 'gethostname', 'getLocalHostName'
  class_primitive 'new', 'new:type:proto:'

  def self.getservbyname(service, proto='tcp')
    # returns a port number as a Fixnum , or raises an error
    #   if the service is not found.
    s = Type.coerce_to(service, String, :to_str)
    p = Type.coerce_to(proto, String, :to_str)
    _getservbyname(s, p)
  end

  def self.getaddrinfo(host, service, family = 0, socktype = 0,
      protocol = 0, flags = 0)
    # implementation in Smalltalk layer is incomplete ,
    #   result will only include a single entry for the specified host
    #   and service, assuming TCP protocol.
    if host.equal?(nil)
      host = 'localhost'
    else
      host = Type.coerce_to(host, String, :to_s)
    end
    if service._isFixnum || service._isString || service.equal?(nil)
      # ok
    else
      service = Type.coerce_to(service, String, :to_s)
    end
    family = Type.coerce_to(family, Fixnum, :to_int)
    socktype = Type.coerce_to(socktype, Fixnum, :to_int)
    protocol = Type.coerce_to(protocol, Fixnum, :to_int)
    flags = Type.coerce_to(flags, Fixnum, :to_int)
    args = [ host, service, family, socktype, protocol, flags ]
    _getaddrinfo( args )
  end
end

class Socket
  class_primitive '_init_socket_constants', '_initSocketConstants'

  self._init_socket_constants  # initialize OS dependent constants,
               #  they are implemented as Transient constants ,
               #  must be in second opening of Socket so Socket's name space
               #  is fully initialized
end

class IPSocket
  class_primitive 'peeraddr', 'peeraddr'
  class_primitive 'getaddress', 'getHostAddressByName:'
end

class TCPSocket

  # open binds a socket to a port and does a blocking connect,
  #  returning a blocking socket.
  class_primitive 'open', 'new:port:'
  class_primitive '_open', 'open:'

  def self.new(host, port)
    self.open(host, port)
  end

  def self.new(host)
    self._open(host)
  end

  def self.open(host)
    self._open(host)
  end

end

class TCPServer
  # accept returns a new socket, having same non-blocking state as receiver
  primitive 'accept', 'accept'

  # new and open  both create a non-blocking listening socket
  class_primitive 'new', 'new:port:'
  class_primitive 'open', 'new:port:'
end






