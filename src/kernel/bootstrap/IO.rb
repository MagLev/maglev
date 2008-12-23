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

  def print(*args)
    # TODO handle non-nil state of $\
    lim = args.length
    n = 0
    while (n < lim)
      elem = args[n]
      unless (elem._isString)
        elem = elem.to_s
      end
      write(elem)
      n = n + 1
    end
  end

  def printf(format, *args)
    s = sprintf(format, *args)
    write(s)
  end

  def putc(arg)
    # TODO putc
    raise NotImplementedError
  end

  def puts(*args)
    lim = args.length
    n = 0
    while (n < lim)
      elem = args[n]
      unless (elem._isString)
        elem = elem.to_s
      end
      write(elem)
      unless (elem[elem.length - 1].equal?(10))
        write("\n")
      end
      n = n + 1
    end
  end

end

