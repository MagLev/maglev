class IO

  module Constants
    FNM_SYSCASE  = 0x00
    FNM_NOESCAPE = 0x01
    FNM_PATHNAME = 0x02
    FNM_DOTMATCH = 0x04
    FNM_CASEFOLD = 0x08

    # For flock(2)
    LOCK_SH      = 0x01         # shared file lock
    LOCK_EX      = 0x02         # exclusive file lock
    LOCK_NB      = 0x04         # non block when locking
    LOCK_UN      = 0x08         # unlock

    # Flags for open(2), fcntl(2)
    APPEND       = 0x0008       # append mode
    CREAT        = 0x0200       # create if not there
    EXCL         = 0x0800       # open with exclusive lock
    NOCTTY       = 0x20000      # open with no controlling tty
    NONBLOCK     = 0x0004       # don't block
    SYNC         = 0x0080       # sync
    TRUNC        = 0x0400       # truncate at open

    RDONLY       = 0x0000       # open readonly
    WRONLY       = 0x0001       # open write only
    RDWR         = 0x0002       # open read and write

    SEEK_SET     = 0            # set file position to offset
    SEEK_CUR     = 1            # set file position to current + offset
    SEEK_END     = 2            # set file position to end of file + offset
  end
  include Constants

  primitive 'binmode', 'binmode'
  primitive 'fileno', 'fileDescriptor'
  primitive 'sync', 'sync'
  primitive 'sync=', 'setSync:'
  primitive 'stat',  'stat'

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

    # If no argument, print $_
    if lim == 0
      args = [$_]
      lim = 1
    end

    n = 0
    while (n < lim)
      elem = args[n]
      if elem.equal?(nil)
        write("nil")
      else
        unless (elem._isString)
          elem = elem.to_s
        end
        write(elem)
      end
      n = n + 1
    end

    sep = $\
    write(sep) unless sep.equal?(nil)
  end

  def printf(format, *args)
    s = sprintf(format, *args)
    write(s)
  end

  def putc(arg)
    # TODO putc
    raise NotImplementedError
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

    # If no parameters, print newline
    if lim == 0
      write("\n")
      return nil
    end

    n = 0
    ts = Thread._recursion_guard_set
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
          puts(elem)
        ensure
          ts.remove(elem)
        end
        line = nil
      else
        line = elem.to_s
      end

      unless line.equal?(nil)
        write(line)
        write("\n") unless line[-1].equal?(10)
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
  def self.read(name, length=Undefined, offset=0)
    offset = Type.coerce_to(offset, Fixnum, :to_int)
    if offset < 0
      raise Errno::EINVAL, "offset must not be negative"
    end
    unless length.equal?(Undefined)
      length = Type.coerce_to(length, Fixnum, :to_int)

      if length < 0
        raise ArgumentError, "length must not be negative"
      end
    end

    File.open(name) do |f|
      f.seek(offset) unless offset.zero?

      if length.equal?(Undefined)
        f.read
      else
        f.read(length)
      end
    end
  end

end

