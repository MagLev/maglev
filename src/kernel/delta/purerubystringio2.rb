class PureRubyStringIO

  # methods with refs to constants defined during purerubystringio.rb

  def each(sep_string=$/, &block)
    unless block_given?
      return IoEnumerator.new(self, :each, sep_string) # for 1.8.7
    end
    if @sio_closed_read ; requireReadable ; end
    s_string = @sio_string
    if sep_string._equal?(nil)
      # start from current position
      pos = @sio_pos
      len = s_string.length - pos
      block.call( s_string[pos, len] )
    else 
      # entire buffer
      s_string.each(sep_string, &block)
    end
    @sio_pos = s_string.length
    self
  end

  alias :each_line :each

  def each_byte(&block)
    unless block_given?
      return IoByteEnumerator.new(self, :each_byte)  # for 1.8.7
    end
    if @sio_closed_read ; requireReadable ; end
    s_string = @sio_string
    len = @sio_string.length
    s_pos = @sio_pos
    while s_pos < len
      # pos must be updated before call to yield
      byte = s_string[s_pos]
      @sio_pos = s_pos + 1
      block.call(byte)
      s_pos = @sio_pos
    end
    self
  end

  def reopen(obj, mode)
    if mode._isInteger 
      if (mode & IO::TRUNC)._not_equal?(0) && obj.frozen?
        raise TypeError, 'cannot truncate frozen input string'
      end
    end
    self._initialize(obj, mode, true)
  end

  def reopen(other_io)
    if other_io._isString
      self._initialize(other_io, IO::RDWR, true )
    else
      other_io = Type.coerce_to(other_io, PureRubyStringIO, :to_strio )
      self._initialize(other_io.string, other_io._mode, true)
    end
  end

  def reopen()
    self._initialize(self.string, "w+", true)
  end

end
