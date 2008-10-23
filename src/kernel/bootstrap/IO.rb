class IO
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

