class PureRubyStringIO  < IO
  # PureRubyStringIO is loaded during bootstrap so we can have multiple 
  # implementations of gets to handle updating of caller's $_

  # For 1.8.7 , deleted appending of caller to exception messages,
  #   in interest of performance.  

  include Enumerable

  IoEnumerator = Enumerable::IoEnumerator
  IoByteEnumerator = Enumerable::IoByteEnumerator

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

  def self.open(string="", mode=MaglevUndefined, &block)
    # mode==MaglevUndefined translated to "r" or "r+" in initialize
    if block_given? then
      begin
        sio = new(string, mode)
        rc = yield(sio)
        rc
      ensure
        sio._string=(nil)
        sio.close
      end
    else
      new(string, mode)
    end
  end

  def <<(obj)
    # gemstone, let @sio_closed_write be checked in syswrite
    if @mode[0]._equal?( ?a ) 
      # in append mode, ignore position and append to the buffer
      if @sio_closed_write ; __require_writable ; end
      str = Maglev::Type.coerce_to(obj, String, :to_s)
      s_string = @sio_string
      s_string << str
      @sio_pos = s_string.size
    else
      write(obj)
    end
    self
  end

  def binmode
    __require_open
    self
  end

  def close
    __require_open
    @sio_closed_read = true
    @sio_closed_write = true
    nil
  end

  def close_read
    raise(IOError, "closing non-duplex IO for reading") if closed_read?
    @sio_closed_read = true
    nil
  end

  def close_write
    raise(IOError, "closing non-duplex IO for writing") if closed_write?
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

  # def each ; end      # in purerubystringio2.rb
  # def each_byte ; end # in purerubystringio2.rb

  def eof
    if @sio_closed_read ; __require_readable ; end
    @sio_pos >= @sio_string.length
  end

  def fcntl(*args)
    raise NotImplementedError, "The fcntl() function is unimplemented on this machine", caller
  end

  def fileno
    nil
  end

  def flush
    self
  end

  def fsync
    if @sio_closed_write ; __require_writable ; end
    0
  end

  def getc
    if @sio_closed_read ; __require_readable ; end
    pos = @sio_pos
    char = @sio_string[pos]
    unless char._equal?(nil)
      @sio_pos = pos +  1
    end
    char
  end

  # def gets ; end # implemented in IO
  #  see also __gets  below

  def initialize(string="", mode=MaglevUndefined)
    self._initialize(string, mode, false)
  end

  def _initialize(string="", mode=MaglevUndefined, is_reopen=false)
    s_buf = Maglev::Type.coerce_to(string, String, :to_str )
    isfrozen = s_buf.frozen?
    @_st_lineNumber = 0  # IV inherited from IO
    append = false
    if mode._isInteger
      if mode._equal?( IO::RDONLY )
        basemode = "r"
      else
        mask = IO::APPEND|IO::TRUNC
        append = (mode & mask)._equal?( IO::APPEND )
        mode = mode & (~ mask ) 
        if mode._equal?( IO::RDWR )
          basemode = "w+" 
        elsif mode._equal?( IO::WRONLY )
          basemode = "w"
        else
          raise ArgumentError, "PureRubyStringIO#initialize: illegal integer mode #{mode}"
        end
      end
      if is_reopen 
        append = true
      end
      @mode = basemode
    elsif mode._equal?(MaglevUndefined)
      basemode = isfrozen ? "r" : "r+"
      @mode = basemode
    else
      mode = Maglev::Type.coerce_to(mode, String, :to_str )
      basemode = mode.dup
      isbinary = basemode.delete!("b")
      if is_reopen 
        append =  basemode != "w"
      end
      if isbinary
        @mode = basemode + "b" # move 'b' to end for cheaper tests in <<
      else
        @mode = basemode
      end
    end
    # @relay = nil  # Gemstone, no longer used
    if basemode == "r"
      @sio_closed_read = false
      @sio_closed_write = true
      @sio_pos = 0
    else
      if isfrozen 
        raise Errno::EACCES , "PureRubyStringIO#initialize, frozen buffer requires mode 'r'"
      end
      if basemode == "r+"
	@sio_closed_read = false
	@sio_closed_write = false
	@sio_pos = 0
      elsif basemode == "w"
	@sio_closed_read = true
	@sio_closed_write = false
	@sio_pos = 0
	unless append ; s_buf.replace("") ; end
      elsif basemode == "w+"
	@sio_closed_read = false
	@sio_closed_write = false
	@sio_pos = 0
	unless append ; s_buf.replace("") ; end
      elsif basemode == "a"
	@sio_closed_read = true
	@sio_closed_write = false
	@sio_pos = s_buf.length
      elsif basemode == "a+"
	@sio_closed_read = false
	@sio_closed_write = false
	@sio_pos = s_buf.length
      else
	raise ArgumentError, "illegal access mode #{mode}", caller
      end
    end
    @sio_string = s_buf 
    self
  end

  def isatty
    false
  end

  def length
    @sio_string.length
  end

  # def lineno ; end # inherited from IO
  # def lineno=(integer) ; end # inherited from IO

  # def reopen ; end # in purerubystringio2.rb

  def path
    nil
  end

  def pid
    nil
  end

  def pos
    #if closed?  # uncomment for 1.8.7
    #  raise IOError, 'IO#pos on closed IO'
    #end
    @sio_pos
  end

  def pos=(integer)
    if closed?
      raise IOError, 'IO#pos on closed IO'
    end
    p = Maglev::Type.coerce_to(integer, Fixnum, :to_int)
    raise( Errno::EINVAL, "argument must be >= 0") if p < 0
    @sio_pos = p
  end

  #  for inherited methods, @sio_closed_write will be checked in syswrite
  #
  # def print(*args) ; end                 # inherited from IO
  # def printf(format_string, *args) ; end # inherited from IO
  # def putc(obj) ; end                    #  inherited from IO
  # def puts(*args) ; end                  # inherited from IO

  def read(length=nil, buffer=nil)
    if @sio_closed_read ; __require_readable ; end
    buf = buffer._equal?(nil) ? "" : Maglev::Type.coerce_to(buffer, String, :to_str)

    s_pos = @sio_pos
    s_string = @sio_string
    bytes_left = (s_string.length - s_pos)
    if length._equal?(nil)
      len = bytes_left
    else
      len = Maglev::Type.coerce_to(length, Fixnum, :to_int)
      return "" if len._equal?(0)  # only in case length = 0 is passed in
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
    r = buf.length._equal?(0) && length._not_equal?(nil) ? nil : buf
    r
  end

  def readchar
    if @sio_closed_read ; __require_readable ; end
    raise(EOFError, "End of file reached") if eof?
    getc
  end

  def readpartial(length, buffer=MaglevUndefined)
    if buffer._equal?(MaglevUndefined)
      res = self.read(length, nil)
    else
      res = self.read(length, buffer)
    end
    if res._equal?(nil)
      raise(EOFError, "End of file reached")
    end
    res
  end

  def getbyte    # added for 1.8.7
    if @sio_closed_read ; __require_readable ; end
    return nil  if eof?
    getc
  end

  def readbyte    # added for 1.8.7
    if @sio_closed_read ; __require_readable ; end
    raise(EOFError, "End of file reached") if eof?
    getc
  end

  def readline(*args)
    raise ArgumentError, 'expected 0 or 1 arg with no block'
  end

  def readline(sep=$/)
    # variant after first gets no bridges   # gemstone
    if @sio_closed_read ; __require_readable ; end
    raise(EOFError, "End of file reached") if eof?
    self.__gets(sep, 0x31)
  end

  def readline
    # variant after first gets no bridges   # gemstone
    if @sio_closed_read ; __require_readable ; end
    raise(EOFError, "End of file reached") if eof?
    self.__gets($/, 0x31)
  end

  def readlines(sep_string=MaglevUndefined)
    if @sio_closed_read ; __require_readable ; end
    if sep_string._equal?(MaglevUndefined)
      sep_string = $/
      return [] if eof?
    elsif sep_string._equal?(nil)
      return [read]
    else
      sep_string = Maglev::Type.coerce_to(sep_string, String, :to_str)
    end
    raise(EOFError, "End of file reached") if eof?
    sep_string = "\n\n" if sep_string.length._equal?(0)
    rc = []
    while ! eof
      rc << gets(sep_string)
    end
    rc
  end

  def _mode
    @mode
  end

  # def reopen ; end   # in delta/purerubystringio2.rb

  def rewind
    @sio_pos = 0
    @_st_lineNumber = 0
  end

  def seek(offset, whence=SEEK_SET)
    if closed?
      raise(IOError, "not opened in seek")
    end
    offset = Maglev::Type.coerce_to(offset, Fixnum, :to_int)
    if whence == SEEK_CUR then
      offset += @sio_pos
    elsif whence == SEEK_END then
      offset += size
    elsif whence == SEEK_SET
      if offset < 0
        raise Errno::EINVAL , 'StringIO#seek, negative seek position not allowed with SEEK_SET'
      end
    else
      raise Errno::EINVAL, 'invalid second arg to StringIO#seek'
    end
    @sio_pos = offset
    0
  end

  def string
    @sio_string
  end

  def _string=(newstring)
    @sio_string = newstring
  end

  def string=(newstring)
    newstring = Maglev::Type.coerce_to(newstring, String, :to_str)
    @sio_string = newstring
    self.rewind
    newstring
  end

  def sync
    true
  end

  def sync=(boolean)
    boolean
  end

  def sysread(length=nil, buffer=nil)
    if @sio_closed_read ; __require_readable ; end
    raise(EOFError, "End of file reached") if eof?
    read(length, buffer)
  end

  def syswrite(string)
    if @sio_closed_write ; __require_writable ; end
    str = Maglev::Type.coerce_to(string, String, :to_s)
    s_string = @sio_string
    my_len = s_string.length
    s_pos = @sio_pos
    arg_len = str.length
    if s_pos._equal?(my_len)
      s_string << str
    elsif s_pos > my_len
      s_string.size=(s_pos) # Pad with nulls
      s_string << str
    else
      s_string[s_pos, arg_len] = str
    end
    @sio_pos = s_pos + arg_len
    arg_len
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
    if @sio_closed_write ; __require_writable ; end
    new_size = Maglev::Type.coerce_to(integer, Fixnum, :to_int)
    raise(Errno::EINVAL, "Invalid argument - negative length") if new_size < 0
    s_string = @sio_string
    old_size = s_string.length
    if new_size < s_string.length
      s_string[ new_size , old_size - new_size ] = ""
    else
      s_string.size=(new_size)
    end
    integer
  end

  def ungetc(integer)
    if @sio_closed_read ; __require_readable ; end
    integer = Maglev::Type.coerce_to(integer, Fixnum, :to_int)
    s_pos = @sio_pos
    if s_pos > 0 then
      @sio_pos = s_pos - 1
      putc(integer)
      @sio_pos -= 1
    end
    nil
  end

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

  def __gets(a_sep_string, vc_frame_arg)
    sep_is_nil = a_sep_string._equal?(nil)
    unless sep_is_nil
      sep_string = Maglev::Type.coerce_to(a_sep_string, String, :to_str)
    end
    if !sep_is_nil && (sep_len = sep_string.size) == 0
      sep_string = "\n\n"
      sep_len = 2
    end

    if @sio_closed_read ; __require_readable ; end
    @_st_lineNumber += 1
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
        res_len = s_pos - pstart + sep_len
      else
        s_len = s_string.length
        if s_pos >= s_len
          @sio_pos = s_pos
          nil.__storeRubyVcGlobal(vc_frame_arg) # store into caller's $_
          return nil  # EOF
        end
        s_pos = s_len.__max(s_pos)
        res_len = s_pos - pstart + 1
      end
      res = s_string[pstart, res_len]
      if (found)
        s_pos += sep_len
      end
      @sio_pos = s_pos
    end
    res.__storeRubyVcGlobal(vc_frame_arg) # store into caller's $_
    res
  end

  def __require_readable
    raise(IOError, "not opened for reading", caller[1..-1]) if @sio_closed_read
  end

  def __require_writable
    raise(IOError, "not opened for writing", caller[1..-1]) if @sio_closed_write
  end

  def __require_open
    raise(IOError, "closed stream", caller[1..-1]) if @sio_closed_read && @sio_closed_write
  end

end
PureRubyStringIO.__freeze_constants
