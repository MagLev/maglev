# -*- coding: utf-8 -*-
class IO

  # FNM*, LOCK*, APPEND ... , SEEK*  , initialized in IO2.rb

  def binmode
    # set receiver to binary mode. Has no effect on Unix
    if self.closed?
      raise IOError , 'binmode called on closed file'
    end
    self
  end



  def bytes()  # added for 1.8.7
    return IoByteEnumerator.new(self, :each_byte)
  end

  def chars()  # added for 1.8.7
    return IoCharEnumerator.new(self, :each_char)
  end

  def dup
    raise NotImplementedError, "IO.dup"
  end

  primitive_nobridge '__fcntl', 'fcntl:with:'

  def each_byte(&block)
    unless block_given?
      return IoByteEnumerator.new(self, :each_byte) # for 1.8.7
    end
    while not eof?
      buf = self.read(4096)
      if buf._equal?(nil)
  return self
      end
      len = buf.size
      if len._equal?(0)
  return self
      end
      n = 0
      while n < len
  block.call( buf[n] )
  n += 1
      end
    end
    self
  end

  def each_char(&block)  
    unless block_given?
      return IoCharEnumerator.new(self, :each_char) # for 1.8.7
    end
    while not eof?
      buf = self.read(4096)
      if buf._equal?(nil)
  return self
      end
      len = buf.size
      if len._equal?(0)
  return self
      end
      n = 0
      while n < len
  str = ' ' 
  str[0] = buf[n]
  block.call( str )
  n += 1
      end
    end
    self
  end

  def __next_line(sep)
    # used by Enumerators
    if sep._equal?(nil)
      self.__contents 
    elsif sep.__size._equal?(0)
      self.__next_paragraph
    else
      self.__next_line( sep )
    end
  end

  def each_line(sep=$/, &block)
    unless block_given?
      return IoEnumerator.new(self, :each_line, sep)  # for 1.8.7
    end
    if sep._equal?(nil)
      block.call( self.__contents )
    else
      sep = Maglev::Type.coerce_to(sep, String, :to_str)
      if sep.__size._equal?(0)
        while not eof?
          para = self.__next_paragraph
          block.call(para)
        end
      else
        while not eof?
          block.call( self.__next_line( sep ))
        end
      end
    end
    self
  end

  alias each each_line 

  def lines(sep=$/)  # added for 1.8.7
    return IoEnumerator.new(self, :each_line, sep)
  end

  def fcntl(op, flags=0)
    # only these operations are supported by __fcntl primitive:
    #   F_GETFD, F_GETFL, F_SETFL, FD_CLOEXEC
    # Socket contains implementation specific to File::NONBLOCK

    op = Maglev::Type.coerce_to(op, Fixnum, :to_int)
    arg = [ flags ]
    status = __fcntl(op, arg )
    if status._equal?(0)
      return 0 # 'set' operation ok
    elsif status._equal?(-1)
      return arg[0] # retrieved value for a 'get' operation
    elsif status < -1
      raise NotImplementedError, "IO#fcntl: operation not implemented: #{op.inspect}"
    else
      Errno.handle(status, 'fcntl failed')
    end
  end

  primitive '__fileno', 'fileDescriptor'

  def fileno
    if self.closed?
      raise IOError, 'cannot get fileno of a closed IO'
    end
    self.__fileno
  end

  alias to_i fileno

  def self.for_fd(fd_int, modestring=nil)
    raise NotImplementedError , 'IO.for_fd not supported yet'
  end

  def self.foreach(filename, sep=$/, &block)
    f = File.open(filename, 'r')
    unless block_given?
      return f.each_line(sep)  # return an Enumerator, for 1.8.7
    end
    f.each_line(sep) { | str | 
      block.call(str)
    }
    nil
  end


  # def fsync ; end # subclass responsibility

  # def getc ; end # subclass responsibility

  def getbyte  # added for 1.8.7
    self.getc
  end 

  def gets(*args)    # [  begin gets implementation
    raise ArgumentError, 'expected 0 or 1 arg, with no block'
  end
  
  def gets(sep_string)
    # variant after first gets no bridges   
    res = self.__gets(sep_string, 0x31)
    res 
  end
  
  def gets
    # variant after first gets no bridges  
    res = self.__gets( $/, 0x31 ) 
    res
  end

  def __gets(a_sep, vcGlobalIdx)
    # __gets maybe reimplemented in subclasses
    if a_sep._equal?(nil)
      res = __contents
      self.__increment_lineno
    else
      sep = Maglev::Type.coerce_to(a_sep, String, :to_str)
      sep_len = sep.length
      if sep_len._equal?(0)
        res = self.eof?  ?  nil : self.__next_paragraph
      else
        res = self.__next_line(sep)
      end
    end
    res.__storeRubyVcGlobal( vcGlobalIdx ) # store into caller's $_
    res
  end

  def __next_paragraph()
    # caller has checked for not eof? 
    para = ''
    while true  # add non-empty lines to result
      str = self.__next_line_to( 10 )
      if str._equal?(nil)
        unless __last_err_code._equal?(0)
          raise IOError , self.__last_err_string  # TODO: Errno::xxx
        end
        return nil
      end
      para << str
      if eof?
        self.__increment_lineno # count paragraphs, not lines
        return para
      end
      ch = self.__peek_byte
      if ch._equal?(10) 
        para << self.read(1)  # add first empty line
        while true   # skip subsequent empty lines 
          break if eof?
          ch = self.__peek_byte
          if ch._not_equal?(10)
            self.__increment_lineno # count paragraphs, not lines
            return para
          end
          self.read(1)
        end
      end
    end
  end

  def __next_line(sep)
    # sep is an Ascii value 0..255, or a String of size 1..128
    res = __next_line_to(sep)
    if res._equal?(nil)
      unless __last_err_code._equal?(0)
        raise IOError , self.__last_err_string  # TODO: Errno::xxx
      end
    else
      self.__increment_lineno
    end
    res
  end

  # reimplement send so $_ maintained for gets
  def send(sym)
    if (sym._equal?(:gets))
      return __gets($/ , 0x31)
    end
    super(sym)
  end

  def send(sym, arg)
    if (sym._equal?(:gets))
      return __gets(arg, 0x31)
    end
    super(sym, arg)
  end

  def __send__(sym)
    if (sym._equal?(:gets))
      return __gets($/ , 0x31)
    end
    super(sym)
  end

  def __send__(sym, arg)
    if (sym._equal?(:gets))
      return __gets(arg , 0x31)
    end
    super(sym, arg)
  end

  # ]   end of gets implementation

  # NOTE: IO#read() is deprecated...perhaps we don't bother? 
  #   read is implemented in subclasses

  def initialize(*args, &block)
    raise NotImplementedError , 'IO.new not supported yet'
  end

  # Returns true if io is associated with a terminal device (tty), and
  # returns false otherwise.
  def isatty
    false
  end
  alias tty? isatty

  def lineno
    if closed?
      raise IOError, 'IO#lineno on a closed IO'
    end
    num = @_st_lineNumber
    if num._equal?(nil)
      num = 0
    end
    num
  end

  def lineno=(integer)
    # per specs, does not alter $. 
    if closed?
      raise IOError, 'IO#lineno= on a closed IO'
    end
    num = Maglev::Type.coerce_to(integer, Fixnum, :to_int)
    @_st_lineNumber = num
    num
  end

  def __increment_lineno
    # to be called by gets implementations
    num = @_st_lineNumber
    if num._equal?(nil)
      num = 0
    end
    num += 1
    @_st_lineNumber = num 
    $. = num
    num
  end

  def self.open(int_fd, mode_string, &block)
    raise StandardError, "IO.open not implemented"
  end

  def pid
    nil  # need to change after self.popen and self.pipe implemented
  end

  def self.popen(cmd, options={}, &block)
    cmd = Maglev::Type.coerce_to(cmd, String, :to_s)
    mode = "r"

    if options._isHash
      #TODO: evaluate options
      mode = options["mode"] ? options["mode"] : "r" 
    else
      mode = Maglev::Type.coerce_to(mode, String, :to_s)
    end

    if cmd[0]._equal?( ?-)
      raise ArgumentError , '"-" prefix not supported by IO.popen' 
    end
    f = File.__popen(cmd, mode);
    if f._isFixnum
      Errno.raise_errno(f, "IO.popen failed");
    end
    if block_given?
      res = block.call( f)
      f.close
      res
    else 
      f
    end
  end

  def <<(anObj)
    unless anObj._isString
      anObj = anObj.to_s
    end
    self.write(anObj)
  end

  # def pos ; end # subclass responsibility

  #  Writes the given object(s) to <em>ios</em>. The stream must be
  #  opened for writing. If the output record separator (<code>$\\</code>)
  #  is not <code>nil</code>, it will be appended to the output. If no
  #  arguments are given, prints <code>$_</code>. Objects that aren't
  #  strings will be converted by calling their <code>to_s</code> method.
  #  With no argument, prints the contents of the variable <code>$_</code>.
  #  Returns <code>nil</code>.
  #
  #     $stdout.print("This is ", 100, " percent.\n")
  #
  #  <em>produces:</em>
  #
  #     This is 100 percent.
  def print(*args)
    # TODO handle non-nil state of $\
    lim = args.length
    if lim == 0 # if no argument, print $_
      usc = self.__getRubyVcGlobal(0x21) # get callers $_
      if usc._equal?(nil)
        usc = "nil"
      end
      write(usc)
    else
      n = 0
      while (n < lim)
        elem = args[n]
        if elem._equal?(nil)
          elem = "nil"
        else
          unless (elem._isString)
            elem = elem.to_s
          end
        end
  write(elem)
        n = n + 1
      end
      sep = $\
      write(sep) unless sep._equal?(nil)
    end
  end

  def print
    # non-bridge zero args variant  so $_ access will work
    usc = self.__getRubyVcGlobal(0x21) # get callers $_
    if usc._equal?(nil)
      usc = "nil"
    end
    write(usc)
  end

  def putc(obj)
    if obj._isString
      str = obj[0,1]  # write first char of arg
    else
      c = Maglev::Type.coerce_to(obj, Integer, :to_int)
      c = c % 256
      str = 'x'
      str[0] = c
    end
    write( str )
    obj
  end

  def printf(format, *args)
    write( sprintf(format, *args) )
    nil
  end

  def putc(obj)
    if obj._isString
      str = obj[0,1]  # write first char of arg
    else
      c = Maglev::Type.coerce_to(obj, Integer, :to_int)
      c = c % 256
      str = 'x'
      str[0] = c
    end
    write( str )
    obj
  end


  #  Writes the given objects to <em>ios</em> as with
  #  <code>IO#print</code>. Writes a record separator (typically a
  #  newline) after any that do not already end with a newline sequence.
  #  If called with an array argument, writes each element on a new line.
  #  If called without arguments, outputs a single record separator.
  #
  #     $stdout.puts("this", "is", "a", "test")
  #
  #  <em>produces:</em>
  #
  #     this
  #     is
  #     a
  #     test
  #
  def puts(*args)
    lim = args.length
    eol = "\n"  # the record separator,  ignores $\  #

    # If no parameters, print newline
    if lim == 0
      write(eol)
      return nil
    end

    n = 0
    ts = Thread.__recursion_guard_set
    while (n < lim)
      suppress = false
      elem = args[n]
      line = ''
      if elem._equal?(nil)
        line = "nil"
      elsif elem._isString
        line = elem
      elsif ts.include?(elem)
        line = "[...]"
      elsif elem._isArray
        begin
          ts.add(elem)
          idx = 0
          elem_size = elem.size
          while idx < elem_size
            puts(elem[idx])
            idx += 1
          end
        ensure
          ts.remove(elem)
        end
        line = nil
      else
        line = elem.to_s
      end
      unless line._equal?(nil)
        write(line)
        write(eol) unless line[-1].eql?(?\n)
      end
      n = n + 1
    end

    nil
  end

  ##
  # Opens the file, optionally seeks to the given offset,
  # then returns length bytes (defaulting to the rest of
  # the file). read ensures the file is closed before returning.
  #
  #  IO.read("testfile")           #=> "This is line one\nThis is line two\nThis is line three\nAnd so on...\n"
  #  IO.read("testfile", 20)       #=> "This is line one\nThi"
  #  IO.read("testfile", 20, 10)   #=> "ne one\nThis is line "
  #
  # Returns the empty string for empty files, unless length is passed, then returns nil

  # since 1.9.3:
  # read(name, [length [, offset]] ) → string click to toggle source
  # read(name, [length [, offset]], open_args) → string

  def self.read(*args)
    raise Errno::EINVAL, "to few arguments" if args.length < 1
    name = args.shift

    case args.length
    when 1
      ex = args.shift      
      if ex.class._equal?(Fixnum)
        length = Maglev::Type.coerce_to(ex, Fixnum, :to_int)
      else
        open_args = ex
      end
    when 2
      length = args.shift
      ex2 = args.shift
      if ex2.class._equal?(Fixnum)
        offset = Maglev::Type.coerce_to(ex2, Fixnum, :to_int)
      else
        open_args = ex2
      end
    when 3
      length = Maglev::Type.coerce_to(args.shift, Fixnum, :to_int)
      offset = Maglev::Type.coerce_to(args.shift, Fixnum, :to_int)
      open_args = args.shift
    end


    offset = 0 if offset._equal?(nil)
    raise Errno::EINVAL, "offset must not be negative" if offset < 0

    read_all_bytes = length._equal?(MaglevUndefined) || length._equal?(nil)
    unless read_all_bytes
      raise ArgumentError, "length must not be negative" if length < 0
    end

    # default return is '' unless length is provided or is 0, then it is
    # nil
    data = (read_all_bytes || length == 0) ? '' : nil
    File.open(name) do |f|
      f.seek(offset) unless offset.zero?
      data = if read_all_bytes
                 f.read
               else
                 f.read(length)
               end
    end
    data
  end

  def self.write(*args)
    raise NotImplementedError, 'Class:write'
  end

  # def read() ; end  # subclass responsibility

  def read_nonblock(*args)
    recv_nonblock(*args)
  end

  def readchar 
    ch = self.getc
    if ch._equal?(nil)
      raise EOFError, 'EOF during readchar'
    end
    ch
  end

  ##
  # Reads a line as with IO#gets, but raises an EOFError on end of file.
  def readline(separator=$/)
    res = self.gets(separator)
    raise EOFError if res._equal?(nil)
    res.__storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def readline
    separator=$/
    res = self.gets(separator)
    raise EOFError if res._equal?(nil)
    res.__storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  ##
  # Reads all of the lines in +io+ and returns them in an +array+.  Lines
  # are separated by the optional +separator+.  The stream must be opened
  # for reading, or an +IOError+ will be raised.
  #
  # f = File.new("testfile")
  # f.readlines               #=> [ "This is line one\n",   "This is line two\n",
  #                           #     "This is line three\n", "And so on...\n" ]
  def readlines(separator=$/)
    r = Array.new
    while ! eof?
      r << gets(separator)
    end
    r
  end

  # def readpartial() ; end  # subclass responsibility

  def __read_into(len, buf)
    res = self.read(len, buf)
    if res._equal?(nil)
      0
    else
      res.size
    end
  end

  # def reopen; end #  subclass responsibility

  # def rewind ; end # subclass responsibility

  # def seek ; end # subclass responsibility

  # sysseek does not work on Sockets yet
  def sysseek(offset, whence = SEEK_SET)
    self.seek(offset, whence)
    self.pos
  end

  def self.select( reads, writes=nil, errs=nil, timeout=nil )
    if timeout._isFixnum
      ms = timeout * 1000
      unless ms._isFixnum && ms >= 0
        raise ArgumentError , "IO#select, timeout not representable as Fixnum milliseconds >=0"
      end
    elsif timeout._not_equal?(nil)
      timeout = Maglev::Type.coerce_to(timeout, Float, :to_f)
      ms = (timeout * 1000.0 ).to_int
      unless ms._isFixnum && ms >= 0
        raise ArgumentError , "IO#select, timeout not representable as Fixnum milliseconds >=0"
      end
    end
    Kernel.__select(reads, writes, errs, *[ ms ])
  end

  primitive 'stat',  'stat'
 
  primitive 'sync', 'sync'

  # sync= has no effect, in Maglev  File and Socket never buffer output
  primitive 'sync=', 'setSync:'  

  def self.sysopen(filename, mode=MaglevUndefined, permission=MaglevUndefined)
    f = File.open(filename, mode, permission)
    f.__fileno 
  end

  # def sysread(length, buffer); end # subclass responsibility

  # def syswrite(length, buffer); end # subclass responsibility

  # def tell ; end # subclass responsibility

  def to_io
    self
  end

  # def ungetc ; end # subclass responsibility

  class StreamCopier
    def initialize(from, to, length, offset)
      @length = length
      @offset = offset

      @from_io, @from = to_io(from, "rb")
      @to_io, @to = to_io(to, "wb")

      @method = read_method @from
    end

    def to_io(obj, mode)
      if obj.kind_of? IO
        flag = true
        io = obj
      else
        flag = false

        if obj._isString
          io = File.open obj, mode
        elsif obj.respond_to? :to_path
          path = Maglev::Type.coerce_to obj, String, :to_path
          io = File.open path, mode
        else
          io = obj
        end
      end

      return flag, io
    end

    def read_method(obj)
      if obj.respond_to? :readpartial
        :readpartial
      else
        :read
      end
    end

    def run
      @from.ensure_open_and_readable
      @to.ensure_open_and_writable

      saved_pos = @from.pos if @from_io

      @from.seek @offset, IO::SEEK_CUR if @offset

      size = @length ? @length : 16384
      bytes = 0

      begin
        while data = @from.send(@method, size, "")
          @to.write data
          bytes += data.size

          break if @length && bytes >= @length
        end
      rescue EOFError
        # done reading
      end

      @to.flush
      return bytes
    ensure
      if @from_io
        @from.pos = saved_pos if @offset
      else
        @from.close
      end

      @to.close unless @to_io
    end
  end

  def self.copy_stream(from, to, max_length=nil, offset=nil)
    StreamCopier.new(from, to, max_length, offset).run
  end
  
  def ensure_open_and_readable
    raise IOError, "not opened for reading" unless !self.closed? and self.__readable
  end

  def ensure_open_and_writable
    raise IOError, "not opened for writing" unless !self.closed? and self.__writable
  end

  def self.binread(*args)
    self.read(*args)
  end

  def self.binwrite(*args)
    self.write(*args)
  end

end
