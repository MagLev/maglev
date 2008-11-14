class IO

#  module Constants
    # MRI, on a Mac, has the following constants defined:
    FNM_SYSCASE  = 0x00
    FNM_NOESCAPE = 0x01
    FNM_PATHNAME = 0x02
    FNM_DOTMATCH = 0x04
    FNM_CASEFOLD = 0x08

    # ...and these as well.
    # TODO: Need to ensure the File inherits these.
    #     LOCK_EX
    #     LOCK_NB
    #     LOCK_SH
    #     LOCK_UN

    #     APPEND
    #     CREAT
    #     EXCL
    #     NOCTTY
    #     NONBLOCK
    #     SYNC
    #     TRUNC

    #     RDONLY
    #     RDWR
    #     WRONLY

    #     SEEK_CUR
    #     SEEK_END
    #     SEEK_SET
#  end
#  include Constants  # NOTE: this won't make the constants visible until
                     # trac ticket 249 is fixed.

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

