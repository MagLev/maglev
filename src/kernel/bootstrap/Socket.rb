#   Maglev does not have a BasicSocket class ,
#   methods that would be in BasicSocket in MRI are implemented in Socket .

class Socket # identical to smalltalk RubySocket , subclass of BasicSocket

  # OS dependent constants initialized by smalltalk code
  #  in Socket>>_initTransientSocketConstants , called from RubyContext>>initTransient .

  # All of the socket constants go into two places: (A) as constants in
  # Socket and (B) as constants in Socket::Constants.
  module Constants
  end

  primitive '__active?', 'isActive'

  # accept implemented only in TCPServer .
  # bind, listen not implemented,
  #   use  TCPServer>>new:port:  to create a listening socket

  primitive '__close', 'close'

  def close
    if self.__active?
      self.__close
    else
      raise IOError, 'already closed'
    end
    nil
  end

  primitive 'close_read' , 'shutdownReading'
  primitive 'close_write' , 'shutdownWriting'
  primitive '__close_readwrite' , 'shutdownReadingAndWriting'

  primitive '__clear_buffer', '_clearReadBuffer'

  def closed?
    ! __active?
  end

  def __contents
    self.read(4096)
  end

  primitive 'connected?', 'isConnected'

  class_primitive 'do_not_reverse_lookup=', 'setNoReverseLookup:'

  def eof?
    ! self.connected?
  end

  def fcntl(op, flags=0)
    # Socket specific implemention for F_SETFL, NONBLOCK
    op = Type.coerce_to(op, Fixnum, :to_int)
    if op._equal?(Fcntl::F_SETFL)
      flags = Type.coerce_to(flags, Fixnum, :to_int)
      if (flags & File::NONBLOCK) != 0
        @_st_isRubyBlocking = false
      else
        @_st_isRubyBlocking = true
      end
      flags = flags & (~ File::NONBLOCK)
      super(op, flags)
    elsif op._equal?(Fcntl::F_GETFL)
      res = super(op, flags)
      if @_st_isRubyBlocking
        res = res | File::NONBLOCK
      end
      res
    else
      super(op, flags)
    end
  end

  def flush
    # nothing to do, no buffering in the VM for socket write operations
    if closed?
      raise IOError, 'cannot flush a closed Socket'
    end
    self
  end

  def fsync
    if closed?
      raise IOError, 'cannot fsync a closed Socket'
    end
    0 # nothing to do for a socket
  end

  class_primitive_nobridge '__getaddrinfo', '_getaddrinfo:'  # one arg , an Array of 6 elements
  class_primitive 'gethostbyname', 'gethostbyname:'
  class_primitive '__getservbyname', 'getservbyname:protocol:'
  class_primitive 'gethostname', 'getLocalHostName'

  def self.getservbyname(service, proto='tcp')
    # returns a port number as a Fixnum , or raises an error
    #   if the service is not found.
    s = Type.coerce_to(service, String, :to_str)
    p = Type.coerce_to(proto, String, :to_str)
    __getservbyname(s, p)
  end

  def self.getaddrinfo(host, service, family = 0, socktype = 0,
                 protocol = 0, flags = 0)
    unless host._equal?(nil)
      host = Type.coerce_to(host, String, :to_s)
    end
    if service._equal?(nil)
      # nil is ok
    elsif service._isFixnum
      serv = service.to_s
    elsif service._isString
      serv = service
    else
      raise TypeError , 'service must be nil, a Fixnum or a String'
    end
    if family._isStringOrSymbol
      fstr = family.to_s    # fix Trac 740
      # constants like AF_INET are undefined during bootstrap, and are defined
      # transiently during normal VM initialization to be OS-dependent values 
      #  by smalltalk code in RubyContext>>_initTransient: .
      if fstr == 'AF_INET'
        family = AF_INET 
      elsif fstr == 'AF_INET6'
        family = AF_INET6 
      else
        raise TypeError, "getaddrinfo: family #{fstr} not supported yet"
      end
    end
    unless family._isFixnum
      raise TypeError , 'getaddrinfo: family must be a Fixnum or a String'
    end
    socktype = Type.coerce_to(socktype, Fixnum, :to_int)
    protocol = Type.coerce_to(protocol, Fixnum, :to_int)
    flags = Type.coerce_to(flags, Fixnum, :to_int)
    args = [ host, serv, family, socktype, protocol, flags]
    if socktype._equal?(0)
      # getaddrinfo() system call may give 'EAI error 9' with hints.ai_socktype==0, 
      #  so iterate explicitly over the common socket types
      args[3] = SOCK_STREAM
    end
    res = self.__getaddrinfo(args)
    if res._isString
      raise SocketError, res
    end
    if socktype._equal?(0)
      args[3] = SOCK_DGRAM
      dg_res = self.__getaddrinfo(args)
      unless dg_res._isString
         res.concat( dg_res)
      end
    end
    return res
  end

  primitive_nobridge '__next_line_to', 'getLine:'

  primitive_nobridge 'getc', 'getByte'

  # def gets; end # implemented in IO

  primitive '__getsockopt', 'getsockopt:name:'

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
    arr = __getsockopt(level, optname)
    if arr._isFixnum
      Errno.raise(arr)
      res = nil
    elsif arr.size._equal?(1)
      arr.pack("i")
    else
      # 2 element Array from SO_RCVTIMEO, SO_SNDTIMEO, or SO_LINGER
      arr.pack("ii")
    end
  end

  primitive '__last_err_code', 'lastErrorCode'
  primitive '__last_err_string', 'lastErrorString'
  class_primitive '__last_err_string', 'lastErrorString'

  class_primitive 'new', 'new:type:proto:'

  primitive '__peek_byte', '_peek'

  def rewind
    self.__clear_buffer # clear the read buffer
    self.lineno=(0)
  end

  # MRI will give errors like 'IOError: sysread for buffered IO'
  # if you mix read: and recv: and attempt recv: on a Socket with
  #  unread buffered input.  Maglev unifies the IO and will deliver data
  # in this case.

  # read exactly length bytes from the socket.
  # If length is undefined,  equivalent to recv(4096)
  # If a Ruby Socket is non-blocking , read will raise EAGAIN
  # if no data is available.
  # If a Ruby Socket is blocking, read will wait for specified number
  # of bytes to be received, allowing other Ruby Threads to run.
  # If EOF encountered on the socket, while return fewer than length bytes
  #  or will return nil.

  def read(length=MaglevUndefined, buffer=MaglevUndefined)
    uu = MaglevUndefined
    if length._equal?(uu)
      self.recv(4096)
    else 
      length = Type.coerce_to(length, Fixnum, :to_int)
      if buffer._equal?(uu) 
        buf = String.__new(length)
        self.__read_into(length, buf, length)
      else
        buffer = Type.coerce_to(a_buffer, String, :to_str)
        self.__read_into(length, buffer, length)
      end
    end
  end

  def read(length)
    length = Type.coerce_to(length, Fixnum, :to_int)
    buf = String.__new(length)
    self.__read_into(length, buf, length)
  end


  primitive '__read_into', 'read:into:minLength:' # returns nil on socket eof

  def readpartial(length, buffer=MaglevUndefined)
    length = Type.coerce_to(length, Fixnum, :to_int)
    if buffer._equal?(MaglevUndefined)
      buf = String.__new(length)
      self.__read_into(length, buf, length)
    else
      buffer = Type.coerce_to(a_buffer, String, :to_str)
      self.__read_into(length, buffer)
    end   
  end

  # def recv(length) ; end #  receive up to length bytes from the socket.
  #  If a Ruby Socket is non-blocking , recv will raise EAGAIN
  #  if no data is available.  The size of the result will be >= 1 and <= length.
  primitive 'recv', 'recv:'

  def seek(offset, whence) # raise not implemented error
    raise NotImplementedError
  end

  # send attempts to write specified string to the underlying socket.
  # If a Ruby Socket is non-blocking , and the underlying C socket is
  # blocked on output,  a send will raise an EAGAIN error.
  # Returns number of bytes written which may be less than requested.
  def send(string, flags, addr)
    unless addr._equal?(nil)
      raise 'addr arg not supported by send'
    end
    unless flags._equal?(0)
      raise 'non-zero flags not supported by send'
    end
    syswrite(string)
  end

  def send(string, flags)
    unless flags._equal?(0)
      raise 'non-zero flags not supported by send'
    end
    syswrite(string)
  end

  primitive_nobridge 'send', 'syswrite:'    # one arg form send(string)

  # explicit bridge methods for send,
  #  send gets no automatic bridge methods in bootstrap
  def send(a1, a2, a3, *args) 
    raise ArgumentError, 'too many args'
  end
  def send(a1, a2, a3, *args, &block)
    if block_given
      raise ArgumentError, 'no block expected'
    end
    self.send(a1, a2, a3, *args)
  end
  def send(a1, a2, a3, &block)
    if block_given
      raise ArgumentError, 'no block expected'
    end
    self.send(a1, a2, 3);
  end
  def send(a1, a2, &block)
    if block_given
      raise ArgumentError, 'no block expected'
    end
    self.send(a1, a2);
  end
  def send(a1, &block)
  end
  def send()
    raise ArgumentError, 'too few args'
  end
  def send(&block)
    raise ArgumentError, 'too few args'
  end
  
  # syswrite attempts to write specified string to the underlying socket.
  # If a Ruby Socket is non-blocking , and the underlying C socket is
  # blocked on output,  a syswrite will raise an EAGAIN error.
  # Returns number of bytes written which may be less than requested.
  primitive_nobridge 'syswrite', 'syswrite:'

  # following 2 are in BasicSocket in Ruby 1.8 , but put in Socket for now,

  primitive_nobridge '__setsockopt', 'setsockopt:name:value:'

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
  #may     struct  in_addr imr_multiaddr;
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
    if optval._equal?(true)
      optval = 1
    elsif optval._equal?(false)
      optval = 0
    end
    status = __setsockopt(level, optname, optval)
    if status._equal?(0)
      return self
    else
      Errno.raise(status)
    end
  end

  def set_blocking(a_boolean)
    # convenience method
    if a_boolean
      fcntl(Fcntl::F_SETFL , fcntl(Fcntl::F_GETFL) | File::NONBLOCK )
    else
      fcntl(Fcntl::F_SETFL , fcntl(Fcntl::F_GETFL) & (~ File::NONBLOCK) )
    end
  end

  def shutdown(how=2)
    how = Type.coerce_to(how, Fixnum, :to_int)
    if how._equal?(2)
      self.__close_readwrite
    elsif how._equal?(1)
      self.close_write
    elsif how._equal?(0)
      self.close_read
    else
      raise ArgumentError , 'arg to shutdown must be 0,1 or 2'
    end
    0
  end

  # Read up to length bytes from the socket into the specified buffer.
  # If a Ruby Socket is non-blocking , sysread will raise EAGAIN
  # if no data is available.
  def sysread(length=MaglevUndefined, a_buffer=MaglevUndefined)
    uu = MaglevUndefined
    if a_buffer._equal?(uu)
      if length._equal?(uu)
        length = nil
      end
      return self.sysread(length)
    end
    if length._equal?(uu) || length._equal?(nil)
      len = 1500
    else
      len = Type.coerce_to(length, Fixnum, :to_int)
      raise ArgumentError, "length must not be negative" if len < 0
    end
    buffer = Type.coerce_to(a_buffer, String, :to_str)
    self.__read_into(len, buffer, 1)  # grows buffer if needed
    buffer
  end

  # If length is nil or not given, read up to 4096 bytes from socket
  # else read up to length bytes from socket .
  # If a Ruby Socket is non-blocking , sysread will raise EAGAIN
  # if no data is available.
  def sysread(length)
    if length._equal?(nil)
      len = 4096
    else
      len = Type.coerce_to(length, Fixnum, :to_int)
      raise ArgumentError, "length must not be negative" if len < 0
    end
    str = self.recv(len)
    if str._equal?(nil)
      raise EOFError, "End of file on socket"
    end
    str
  end

  primitive 'ungetc', 'ungetByte:'

  # << uses buffered IO semantics and will wait for socket to transmit all the data.
  # never raises EAGAIN.
  primitive_nobridge '<<', 'write:'

  # write uses buffered IO semantics and will wait for socket to transmit all the data
  # never raises EAGAIN.
  primitive_nobridge 'write', 'write:'

end # ]


class IPSocket  # < Socket in Smalltalk bootstrap
  class_primitive '__getaddress', 'getHostAddressByName:'

  def self.getaddress(hostname)
    hostname = Type.coerce_to(hostname, String, :to_str)  
    addr = self.__getaddress(hostname)
    if addr._equal?(nil)
      detail = self.__last_err_string
      raise SocketError , (detail._equal?(nil) ? 'no details' : detail)
    end
    addr
  end

  primitive 'peeraddr', 'rubyPeerAddress'
  primitive 'addr', 'rubyAddress'
end

class TCPSocket  # < IPSocket in Smalltalk bootstrap

  # open binds a socket to a port and does a blocking connect,
  #  returning a blocking socket.
  class_primitive '__new', 'new:port:'
  class_primitive '__open', 'open:'

  def self.new(host, port=MaglevUndefined)
    if host._equal?(nil)
      host = 'localhost'
    end
    if port._equal?(MaglevUndefined)
      res = self.__open(host)
    else
      res = self.__new(host, port)
    end
    if res._equal?(nil)
      raise Errno::ECONNREFUSED
    end
    res
  end

  def self.open(host, port=MaglevUndefined)
    self.new(host, port)
  end

end

class UDPSocket # < IPSocket in Smalltalk bootstrap
  class_primitive '__new', 'new'

  def self.new(family=nil)
   
    unless family._equal?(nil)
      unless family == AF_INET 
        raise 'only AF_INET supported'
      end
    end
    self.__new
  end

  def self.open(family=nil)
    self.new(family)
  end 

  primitive '__bind', 'bind:port:'

  def bind(hostname, port)
    # hostname  '<broadcast>' maps to INADDR_BROADCAST
    # hostname   ''   maps to INADDR_ANY
    # port == nil maps to a random port
    if hostname._equal?(nil)
      hostname = ''
    else
      hostname = Type.coerce_to(hostname, String, :to_str)  
    end
    unless port._equal?(nil)
      port = Type.coerce_to(port, Fixnum, :to_int)
    end
    self.__bind(hostname, port)
    0
  end

  primitive '__connect', 'connectTo:on:'

  def connect(hostname, port)
    # hostname  '<broadcast>' maps to INADDR_BROADCAST
    # hostname   ''   maps to INADDR_ANY
    hostname = Type.coerce_to(hostname, String, :to_str)
    port = Type.coerce_to(port, Fixnum, :to_int)
    status = self.__connect(port, hostname)
    if status._equal?(false)
      raise Errno::ECONNREFUSED
    end
    self
  end

  # def recvfrom(length, flags) ; end  
  #   non-zero flags not implemented yet
  primitive '__recvfrom' , 'recvfrom:flags:'

  def recvfrom(length, flags=0)
    # non-zero flags not supported yet
    self.__recvfrom(length, flags)
  end

  def recvfrom(length)
    self.__recvfrom(length, 0)
  end

  def recvfrom_nonblock(length, flags=0)
    # non-zero flags not supported yet
    @_st_isRubyBlocking = false  # inline set_blocking(false)
    self.__recvfrom(length, flags)
  end

  def recvfrom_nonblock(length)
    @_st_isRubyBlocking = false
    self.__recvfrom(length, 0)
  end

  # def send(string, flags) ; end # inherited
  
  primitive_nobridge '__sendto*', '_send:flags:host:port:'

  def send(string, flags, host, *args)
    unless args.__size._equal?(1)
      raise ArgumentError, 'too many args'
    end
    # args[0] is port
    self.__sendto(string, flags, host, *args)
  end

end

class TCPServer   # < TCPSocket in Smalltalk bootstrap
  # accept returns a new socket, having same non-blocking state as receiver
  primitive 'accept', 'accept'

  def accept_nonblock
    @_st_isRubyBlocking = false  # inline set_blocking(false)
    self.accept
  end

  primitive '__listen', 'makeListener:'

  def listen(queue_size=10)
    queue_size = Type.coerce_to(queue_size, Fixnum, :to_int)
    if queue_size < 1 || queue_size > 1000
      raise ArgumentError , 'arg to listen must be >= 1 and <= 1000'
    end
    self.__listen(queue_size) 
    0
  end

  # creates a non-blocking listening socket
  class_primitive '__new', 'new:port:'

  def self.new( a1, a2=MaglevUndefined)
    if a2._equal?(MaglevUndefined)
      port = a1
      if port._isString
        self.new(port , nil)
      elsif port._isFixnum
         self.__new( 'localhost', port == 0 ? nil : port )
      else
         raise TypeError , 'expected a Fixnum port number or String hostname'
      end
    else
      hostname = a1
      port = a2
      # port may be nil or 0 to get a random port
      port = port._equal?(0) ? nil : port
      hostname = Type.coerce_to(hostname, String, :to_str)
      if hostname.length._equal?(0)
        self.__new( 'localhost' , port)
      else
        self.__new( hostname, port )
      end
    end
  end

  def self.open(a1, a2=MaglevUndefined)
    self.new(a1, a2)
  end
end

class UNIXSocket # < Socket in Smalltalk bootstrap
  
  class_primitive '__new', 'new'

  primitive_nobridge '__connect', 'connect:'

  def self.open(path)
    sock = self.__new
    path = Type.coerce_to(path, String, :to_str) 
    unless sock.__connect(path)
      raise SocketError , "UNIXSocket connect failed for #{path}"
    end
    sock
  end

  class << self
    alias new open
  end

  # returns a pair of connected sockets with no path
  # def self.pair ; end 
  class_primitive 'pair', 'pair'
    
  # returns [ af_family, path ]
  # def addr; end 
  primitive 'addr', 'address'

  # returns a String (for a Server) or empty string (for a client)
  def path
    a = self.addr
    a[1]
  end

  primitive '__peerpath', '_peerPath'

  # returns [ af_family, path ] 
  def peeraddr
    p = self.__peerpath 
    if p._equal?(nil)
      return self.addr
    end
    [ "AF_UNIX", p ]
  end

  primitive '__recvfrom_flags' , 'recvfrom:flags:'

  def __recvfrom(length, flags)
    res = self.__recvfrom_flags(length, flags)
    from = res[1]
    if from[0]._equal?(nil)
      res[1] = ["AF_UNIX", ""]
    end
    res
  end

  # returns [ data , [ af_family, path]]
  # def recvfrom(length, flags) ; end  
  #   non-zero flags not implemented yet
  def recvfrom(length, flags=0)
    # non-zero flags not supported yet
    self.__recvfrom(length, flags)
  end

  def recvfrom(length)
    self.__recvfrom(length, 0)
  end

  def recvfrom_nonblock(length, flags=0)
    # non-zero flags not supported yet
    @_st_isRubyBlocking = false  # inline set_blocking(false)
    self.__recvfrom(length, flags)
  end

  def recvfrom_nonblock(length)
    @_st_isRubyBlocking = false
    self.__recvfrom(length, 0)
  end

end

class UNIXServer # < UNIXSocket in Smalltalk bootstrap

  primitive '__listen', 'makeListener:'

  primitive_nobridge '__bind', 'bind:'

  def self.open(path)
    sock = self.__new
    path = Type.coerce_to(path, String, :to_str)
    sock.__bind(path)
    sock.__listen(10)  # default queue length 10
    sock
  end

  class << self
    alias new open
  end

  primitive 'accept', 'accept'

  def peeraddr
    raise ArgumentError , 'peeraddr not supported on UNIXServer sockets'
  end

  def accept_nonblock
    @_st_isRubyBlocking = false  # inline set_blocking(false)
    self.accept
  end

end
