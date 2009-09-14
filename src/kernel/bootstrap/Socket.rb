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

  # OS dependent constants initialized by smalltalk code
  #  in Socket>>_initTransientSocketConstants , called from RubyContext>>initTransient .

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
    raise EOFError if s.equal?(nil)
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

  # Gets a socket option. These are protocol and system specific, see your
  # local sytem documentation for details. The option is returned as
  # a String with the data being the binary value of the socket option.
  #
  # === Parameters
  #  # +level+ is an integer, usually one of the SOL_ constants such as
  #   Socket::SOL_SOCKET, or a protocol level.
  #  # +optname+ is an integer, usually one of the SO_ constants, such
  #   as Socket::SO_REUSEADDR.
  #
  # === Examples
  #
  # Some socket options are integers with boolean values, in this case
  # #getsockopt could be called like this:
  #   optval = sock.getsockopt(Socket::SOL_SOCKET,Socket::SO_REUSEADDR)
  #   optval = optval.unpack "i"
  #   reuseaddr = optval[0] == 0 ? false : true
  #
  # Some socket options are integers with numeric values, in this case
  # #getsockopt could be called like this:
  #   optval = sock.getsockopt(Socket::IPPROTO_IP, Socket::IP_TTL)
  #   ipttl = optval.unpack("i")[0]
  #
  # Option values may be structs. Decoding them can be complex as it involves
  # examining your system headers to determine the correct definition. An
  # example is a +struct linger+, which may be defined in your system headers
  # as:
  #   struct linger {
  #     int l_onoff;
  #     int l_linger;
  #   };
  #
  # In this case #getsockopt could be called like this:
  #   optval =  sock.getsockopt(Socket::SOL_SOCKET, Socket::SO_LINGER)
  #   onoff, linger = optval.unpack "ii"
  def getsockopt(level, optname)
    arr = _getsockopt(level, optname)
    if arr._isFixnum
      Errno.raise(arr)
      res = nil
    elsif arr.size.equal?(1)
      arr.pack("i")
    else
      # 2 element Array from SO_RCVTIMEO, SO_SNDTIMEO, or SO_LINGER
      arr.pack("ii")
    end
  end

  # Sets a socket option. These are protocol and system specific, see your
  # local sytem documentation for details.
  #
  # === Parameters
  # * +level+ is an integer, usually one of the SOL_ constants such as
  #   Socket::SOL_SOCKET, or a protocol level.
  # * +optname+ is an integer, usually one of the SO_ constants, such
  #   as Socket::SO_REUSEADDR.
  # * +optval+ is the value of the option, it is passed to the underlying
  #   setsockopt() as a pointer to a certain number of bytes. How this is
  #   done depends on the type:
  #   - Fixnum: value is assigned to an int, and a pointer to the int is
  #     passed, with length of sizeof(int).
  #   - true or false: 1 or 0 (respectively) is assigned to an int, and the
  #     int is passed as for a Fixnum. Note that +false+ must be passed,
  #     not +nil+.
  #   - String: the string's data and length is passed to the socket.
  #
  # === Examples
  #
  # Some socket options are integers with boolean values, in this case
  # #setsockopt could be called like this:
  #   sock.setsockopt(Socket::SOL_SOCKET,Socket::SO_REUSEADDR, true)
  #
  # Some socket options are integers with numeric values, in this case
  # #setsockopt could be called like this:
  #   sock.setsockopt(Socket::IPPROTO_IP, Socket::IP_TTL, 255)
  #
  # Option values may be structs. Passing them can be complex as it involves
  # examining your system headers to determine the correct definition. An
  # example is an +ip_mreq+, which may be defined in your system headers as:
  #   struct ip_mreq {
  #     struct  in_addr imr_multiaddr;
  #     struct  in_addr imr_interface;
  #   };
  #
  # In this case #setsockopt could be called like this:
  #   optval =  IPAddr.new("224.0.0.251") + Socket::INADDR_ANY
  #   sock.setsockopt(Socket::IPPROTO_IP, Socket::IP_ADD_MEMBERSHIP, optval)
  #
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

class IPSocket
  class_primitive 'getaddress', 'getHostAddressByName:'
  primitive 'peeraddr', 'rubyPeerAddress'
  primitive 'addr', 'rubyAddress'
end

class TCPSocket

  # open binds a socket to a port and does a blocking connect,
  #  returning a blocking socket.
  class_primitive 'open', 'new:port:'
  class_primitive '_open', 'open:'

  def self.new(host, port)
    if host.equal?(nil)
      host = 'localhost'
    end
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

  # creates a non-blocking listening socket
  class_primitive '_new', 'new:port:'

  def self.new( port )
    if port._isString
      self.new(port , nil)
    elsif port._isFixnum
       self._new( 'localhost', port)
    else
       raise TypeError , 'expected a Fixnum port number or String hostname'
    end
  end

  def self.new( hostname, port)
    # port may be nil to get a random port
    if hostname.length.equal?(0)
      self._new( 'localhost' , port)
    else
      self._new( hostname, port )
    end
  end

  def self.open( port )
    self.new(port)
  end

  def self.open( hostname, port)
    self.new(hostname, port)
  end

end
