class IO

  # FNM*, LOCK*, APPEND ... , SEEK*  , initialized in IO2.rb

  def binmode
    # set receiver to binary mode. Has no effect on Unix
    if self.closed?
      raise IOError , 'binmode called on closed file'
    end
    self
  end

  primitive 'fileno', 'fileDescriptor'
  primitive 'sync', 'sync'
  primitive 'sync=', 'setSync:'
  primitive 'stat',  'stat'

  primitive_nobridge '__fcntl', 'fcntl:with:'

  def fcntl(op, flags=0)
    # only these operations are supported by __fcntl primitive:
    #   F_GETFD, F_GETFL, F_SETFL, FD_CLOEXEC
    # Socket contains implementation specific to File::NONBLOCK

    op = Type.coerce_to(op, Fixnum, :to_int)
    arg = [ flags ]
    status = __fcntl(op, arg )
    if status.equal?(0)
      return 0 # 'set' operation ok
    elsif status.equal?(-1)
      return arg[0] # retrieved value for a 'get' operation
    elsif status < -1
      raise NotImplementedError, 'fcntl operation not implemented'
    else
      Errno.handle(status, 'fcntl failed')
    end
  end

  def <<(anObj)
    unless anObj._isString
      anObj = anObj.to_s
    end
    self.write(anObj)
  end

  # NOTE: IO#read() is deprecated...perhaps we don't bother?

  # Returns true if io is associated with a terminal device (tty), and
  # returns false otherwise.
  def isatty
    false
  end
  alias tty? isatty

  def self.popen(cmd, mode="r", &block)
    raise NotImplementedError
  end

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
      if usc.equal?(nil)
        usc = "nil"
      end
      write(usc)
    else
      n = 0
      while (n < lim)
        elem = args[n]
        if elem.equal?(nil)
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
      write(sep) unless sep.equal?(nil)
    end
  end

  def print
    # non-bridge zero args variant  so $_ access will work
    usc = self.__getRubyVcGlobal(0x21) # get callers $_
    if usc.equal?(nil)
      usc = "nil"
    end
    write(usc)
  end

  def putc(obj)
    if obj._isString
      str = obj[0,1]  # write first char of arg
    else
      c = Type.coerce_to(obj, Integer, :to_int)
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
      c = Type.coerce_to(obj, Integer, :to_int)
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
      if elem.equal?(nil)
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
      unless line.equal?(nil)
        write(line)
        write(eol) unless line[-1].equal?(10)
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
  def self.read(name, length=Undefined, an_offset=0)
    offset = if an_offset.nil?
               0
             else
               Type.coerce_to(an_offset, Fixnum, :to_int)
             end
    raise Errno::EINVAL, "offset must not be negative" if offset < 0

    read_all_bytes = length.equal?(Undefined) || length.nil?
    unless read_all_bytes
      length = Type.coerce_to(length, Fixnum, :to_int)
      raise ArgumentError, "length must not be negative" if length < 0
    end

    # default return is '' unless length is provided or is 0, then it is
    # nil
    data = (read_all_bytes || length == 0) ? '' : nil
    File.open(name) do |f|
      unless f.eof?
        f.seek(offset) unless offset.zero?
        data = if read_all_bytes
                 f.read
               else
                 f.read(length)
               end
      end
    end
    data
  end

  def lineno
    num = @lineNumber
    if num.equal?(nil)
      num = 0
    end
    num
  end

  def lineno=(integer)
    num = Type.coerce_to(integer, Fixnum, :to_i)
    if num < 0
      raise ArgumentError, 'IO#lineno= expects Integer >= 0'
    end
    @lineNumber = num
    num
  end

  def __increment_lineno
    # to be called by gets implementations
    num = @lineNumber
    if num.equal?(nil)
      num = 0
    end
    @lineNumber = num + 1
  end

  ##
  # Reads a line as with IO#gets, but raises an EOFError on end of file.
  def readline(separator=$/)
    res = self.gets(separator)
    raise EOFError if res.nil?
    res.__storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def readline
    separator=$/
    res = self.gets(separator)
    raise EOFError if res.nil?
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

  def __read_into(len, buf)
    res = self.read(len, buf)
    if res.equal?(nil)
      0
    else
      res.size
    end
  end

  def self.select( reads, writes=nil, errs=nil, timeout=nil )
    if timeout._isFixnum
      ms = timeout * 1000
      unless ms._isFixnum && ms >= 0
        raise ArgumentError , "IO#select, timeout not representable as Fixnum milliseconds >=0"
      end
    elsif timeout._not_equal?(nil)
      timeout = Type.coerce_to(timeout, Float, :to_f)
      ms = (timeout * 1000.0 ).to_int
      unless ms._isFixnum && ms >= 0
        raise ArgumentError , "IO#select, timeout not representable as Fixnum milliseconds >=0"
      end
    end
    Kernel.__select(reads, writes, errs, *[ ms ])
  end
end
