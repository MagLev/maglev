class PureRubyStringIO < IO
  # loaded during bootstrap so we can have multiple implementations of gets
  #   to handle updating of caller's $_

  include Enumerable

  SEEK_CUR = IO::SEEK_CUR
  SEEK_END = IO::SEEK_END
  SEEK_SET = IO::SEEK_SET

  # Gemstone, relayMethods not used
  # @@relayMethods = [
  #  :<<, :all?, :any?, :binmode, :close, :close_read, :close_write, :closed?, :closed_read?,
  #  :closed_write?, :collect, :detect, :each, :each_byte, :each_line, :each_with_index,
  #  :entries, :eof, :eof?, :fcntl, :fileno, :find, :find_all, :flush, :fsync, :getc, :gets,
  #  :grep, :include?, :inject, :isatty, :length, :lineno, :lineno=, :map, :max, :member?,
  #  :min, :partition, :path, :pid, :pos, :pos=, :print, :printf, :putc, :puts, :read,
  #  :readchar, :readline, :readlines, :reject, :rewind, :seek, :select, :size, :sort,
  #  :sort_by, :string, :string=, :sync, :sync=, :sysread, :syswrite, :tell, :truncate, :tty?,
  #  :ungetc, :write, :zip]

  def self.open(string="", mode="r+")
    if block_given? then
      sio = new(string, mode)
      rc = yield(sio)
      sio.close
      rc
    else
      new(string, mode)
    end
  end

  def <<(obj)
    if @sio_closed_write ; requireWritable ; end
    write(obj)
    self
  end

  def binmode
    self
  end

  def close
    requireOpen
    @sio_closed_read = true
    @sio_closed_write = true
    nil
  end

  def close_read
    raise IOError, "closing non-duplex IO for reading", caller if closed_read?
    @sio_closed_read = true
    nil
  end

  def close_write
    raise IOError, "closing non-duplex IO for writing", caller if closed_write?
    @sio_closed_write = true
    nil
  end

  def closed?

    closed_read? && closed_write?
  end

  def closed_read?
    @sio_closed_read
  end

  def closed_write?
    @sio_closed_write
  end

  def each(sep_string=$/, &block)
    if @sio_closed_read ; requireReadable ; end
    @sio_string.each(sep_string, &block)
    @sio_pos = @sio_string.length
  end

  def each_byte(&block)
    if @sio_closed_read ; requireReadable ; end
    len = @sio_string.length
    s_pos = @sio_pos
    while s_pos < len
      # pos must be updated before call to yield
      byte = @sio_string[s_pos]
      @sio_pos = s_pos + 1
      block.call(byte)
      s_pos = @sio_pos
    end
  end

  def eof
    if @sio_closed_read ; requireReadable ; end
    @sio_pos >= @sio_string.length
  end

  def fcntl(integer_cmd, arg)
    raise NotImplementedError, "The fcntl() function is unimplemented on this machine", caller
  end

  def fileno
    nil
  end

  def flush
    self
  end

  def fsync
    0
  end

  def getc
    if @sio_closed_read ; requireReadable ; end
    pos = @sio_pos
    char = @sio_string[pos]
    unless char.equal?(nil)
      @sio_pos = pos +  1
    end
    char
  end

  # Gemstone, install variants of gets to store into caller's $_
  #   see private  def _gets; ...  at end of file
  def gets(*args)
    raise ArgumentError, 'expected 0 or 1 arg'
  end

  def gets(sep_string=$/)
    # variant after first gets no bridges   # gemstone
    res = self._gets(sep_string, 0x31)
    res
  end

  def gets
    # variant after first gets no bridges   # gemstone
    res = self._gets( $/, 0x31 )
    res
  end

  def initialize(string="", mode="r+")
    @sio_string = string.to_s
    @sio_lineno = 0
    @mode = mode
    # @relay = nil  # Gemstone, no longer used
    case mode.delete("b")
    when "r"
      @sio_closed_read = false
      @sio_closed_write = true
      @sio_pos = 0
    when "r+"
      @sio_closed_read = false
      @sio_closed_write = false
      @sio_pos = 0
    when "w"
      @sio_closed_read = true
      @sio_closed_write = false
      @sio_pos = 0
      @sio_string.replace("")
    when "w+"
      @sio_closed_read = false
      @sio_closed_write = false
      @sio_pos = 0
      @sio_string.replace("")
    when "a"
      @sio_closed_read = true
      @sio_closed_write = false
      @sio_pos = @sio_string.length
    when "a+"
      @sio_closed_read = false
      @sio_closed_write = false
      @sio_pos = @sio_string.length
    else
      raise ArgumentError, "illegal access mode #{mode}", caller
    end
  end

  def isatty
    flase
  end

  def length
    @sio_string.length
  end

  def lineno
    @sio_lineno
  end

  def lineno=(integer)
    @sio_lineno = integer
  end

  def path
    nil
  end

  def pid
    nil
  end

  def pos
    @sio_pos
  end

  def pos=(integer)
    raise Errno::EINVAL, "Invalid argument", caller if integer < 0
    @sio_pos = integer
  end

  def print(*args)
    if @sio_closed_write ; requireWritable ; end
    args.unshift($_) if args.empty
    args.each { |obj| write(obj) }
    write($\) unless $\.equal?(nil)
    nil
  end

  def printf(format_string, *args)
    if @sio_closed_write ; requireWritable ; end
    write format(format_string, *args)
    nil
  end

  def putc(obj)
    if @sio_closed_write ; requireWritable ; end
    write(obj.is_a?(Numeric) ? sprintf("%c", obj) : obj.to_s[0..0])
    obj
  end

  def puts(*args)
    if @sio_closed_write ; requireWritable ; end
    args.unshift("") if args.empty?
    args.each { |obj|
      write obj
      write $/
    }
    nil
  end

  def read(length=nil, buffer=nil)
    if @sio_closed_read ; requireReadable ; end
    buf = buffer.equal?(nil) ? "" : Type.coerce_to(buffer, String, :to_str)

    s_pos = @sio_pos
    s_string = @sio_string
    bytes_left = (s_string.length - s_pos)
    if length.equal?(nil)
      len = bytes_left
    else
      len = Type.coerce_to(length, Fixnum, :to_int)
      return "" if len.equal?(0)  # only in case length = 0 is passed in
    end
    if bytes_left < len
      len = bytes_left
    end
    if len < 0
      raise ArgumentError, "negative length #{len} given", caller
    end
    pstart = s_pos
    s_pos += len
    @sio_pos = s_pos
    buf.replace(s_string[pstart..(s_pos - 1)])
    r = buf.empty? && !length.equal?(nil) ? nil : buf
    r
  end

  def readchar
    if @sio_closed_read ; requireReadable ; end
    raise EOFError, "End of file reached", caller if eof?
    getc
  end

  def readline(*args)
    raise ArgumentError, 'expected 0 or 1 arg'
  end

  def readline(sep=$/)
    # variant after first gets no bridges   # gemstone
    if @sio_closed_read ; requireReadable ; end
    raise EOFError, "End of file reached", caller if eof?
    self._gets(sep, 0x31)
  end

  def readline
    # variant after first gets no bridges   # gemstone
    if @sio_closed_read ; requireReadable ; end
    raise EOFError, "End of file reached", caller if eof?
    self._gets($/, 0x31)
  end

  def readlines(sep_string=Undefined)
    if @sio_closed_read ; requireReadable ; end
    if sep_string.equal?(Undefined)
      sep = $/
      return [] if eof?
    elsif sep_string.equal?(nil)
      return [read]
    else
      sep = Type.coerce_to(sep_string, String, :to_str)
    end
    raise EOFError, "End of file reached", caller if eof?
    sep = "\n\n" if sep.empty?
    rc = []
    while ! eof
      rc << gets(sep)
    end
    rc
  end

  def _mode
    @mode
  end

  def reopen(other_io, mode_arg=nil)
    # Gemstone edits to delete use of ObjectSpace
    if other_io._isString
      raise ArgumentError, 'wrong number of arguments (1 for 2)', caller if mode_arg.equal?(nil)
      self.initialize(other_io, mode_arg)
    elsif other_io.is_a?(self.class) then
      raise ArgumentError, 'wrong number of arguments (2 for 1)', caller if ! mode_arg.equal?(nil)
      self.initialize(other_io.string, other_io._mode)
    else
      raise ArgumentError, 'unsupported kind of reopen'
    end
    self
  end

  def rewind
    @sio_pos = 0
    @sio_lineno = 0
  end

  def seek(offset, whence=SEEK_SET)
    if whence == SEEK_CUR then
      offset += @sio_pos
    elsif whence == SEEK_END then
      offset += size
    end
    @sio_pos = offset
  end

  def string
    @sio_string
  end

  def string=(newstring)
    @sio_string = newstring
  end

  def sync
    true
  end

  def sync=(boolean)
    boolean
  end

  def sysread(length=nil, buffer=nil)
    if @sio_closed_read ; requireReadable ; end
    raise EOFError, "End of file reached", caller if eof?
    read(length, buffer)
  end

  def syswrite(string)
    if @sio_closed_write ; requireWritable ; end
    str = Type.coerce_to(string, String, :to_s)
    s_string = @sio_string
    my_len = s_string.length
    s_pos = @sio_pos
    if s_pos > my_len
      # Pad with nulls
      s_string << ("\000" * (s_pos - my_len))
    end
    s_string[s_pos, str.length] = str
    str_siz = str.size
    @sio_pos = s_pos + str_siz
    str_siz
  end

  # In ruby 1.8.4 truncate differs from the docs in two ways.

  # First, if an integer greater that the length is given then the string
  # is expanded to the new integer length. As this expansion seems to
  # contain junk characters instead of nulls I suspect this may be a flaw
  # in the C code which could cause a core dump if abused/used.
  #
  # Second, the documentation states that truncate returns 0. It returns
  # the integer instead.  This implementation follows the documentation in
  # the first instance as I suspect this will be fixed in the C code. In
  # the second instance, it follows the actions of the C code instead of
  # the docs.  This was decided as it causes no immedeate harm and this
  # ruby implentation is to be as compatable as possible with the C
  # version. Should the C version change to match the docs the ruby version
  # will be simple to update as well.
  def truncate(integer)
    if @sio_closed_write ; requireWritable ; end
    raise Errno::EINVAL, "Invalid argument - negative length", caller if integer < 0
    s_string = @sio_string
    s_string[ s_string.length._max(integer) ..-1] = ""
    integer
  end

  def ungetc(integer)
    if @sio_closed_write ; requireWritable ; end
    s_pos = @sio_pos
    if s_pos > 0 then
      @sio_pos = s_pos - 1
      putc(integer)
      @sio_pos -= 1
    end
  end

  alias :each_line :each
  alias :eof? :eof
  alias :size :length
  alias :tty? :isatty
  alias :tell :pos
  alias :write :syswrite

  # Gemstone,  relayMethods not used
  # protected
  # @@relayMethods.each { |name|
  #  alias_method("original_#{name}".to_sym, name)
  #  protected "original_#{name}".to_sym
  #}

  private

  def _gets(a_sep_string, vc_frame_arg)
    sep_is_nil = a_sep_string.equal?(nil)
    unless sep_is_nil
      sep_string = Type.coerce_to(a_sep_string, String, :to_str)
    end
    if !sep_is_nil && sep_string.size == 0
      sep_string = "\n\n"
    end

    if @sio_closed_read ; requireReadable ; end
    @sio_lineno += 1
    s_pos = @sio_pos
    pstart = s_pos
    s_string = @sio_string
    if sep_is_nil
      res = s_string[@sio_pos..-1]
      @sio_pos = s_string.length
    else
      found = s_string.index(sep_string, s_pos)
      if found
        s_pos = found
      else
        s_len = s_string.length
        if s_pos >= s_len
          @sio_pos = s_pos
          nil._storeRubyVcGlobal(vc_frame_arg) # store into caller's $_
          return nil  # EOF
        end
        s_pos = s_len._max(s_pos)
      end
      res = s_string[pstart..s_pos]
      if (found)
        s_pos += sep_string.length
      end
      @sio_pos = s_pos
    end
    res._storeRubyVcGlobal(vc_frame_arg) # store into caller's $_
    res
  end

  def requireReadable
    raise IOError, "not opened for reading", caller[1..-1] if @sio_closed_read
  end

  def requireWritable
    raise IOError, "not opened for writing", caller[1..-1] if @sio_closed_write
  end

  def requireOpen
    raise IOError, "closed stream", caller[1..-1] if @sio_closed_read && @sio_closed_write
  end

end
