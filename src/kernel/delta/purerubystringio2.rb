class PureRubyStringIO

  # methods with refs to constants defined during purerubystringio.rb

  def each(sep_string=$/, &block)
    unless block_given?
      return IoEnumerator.new(self, :each, sep_string) # for 1.8.7
    end
    if @sio_closed_read ; __require_readable ; end
    s_string = @sio_string
    if sep_string._equal?(nil)
      # from current position
      pos = @sio_pos
      len = s_string.length - pos
      s_string[pos, len].each( &block)
    else 
      # entire buffer
      #s_string.each(sep_string, &block)
      # 1.8.7, from current position
      pos = @sio_pos
      len = s_string.length - pos
      s_string[pos, len].each(sep_string, &block)
    end
    @sio_pos = s_string.length
    self
  end

  alias each_line each
  alias lines     each  # for 1.8.7

  def each_byte(&block)
    unless block_given?
      return IoByteEnumerator.new(self, :each_byte)  # for 1.8.7
    end
    if @sio_closed_read ; __require_readable ; end
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

  alias bytes each_byte

  def each_char(&block)
    unless block_given?
      return IoCharEnumerator.new(self, :each_char)  # for 1.8.7
    end
    if @sio_closed_read ; __require_readable ; end
    s_string = @sio_string
    len = @sio_string.length
    s_pos = @sio_pos
    while s_pos < len
      # pos must be updated before call to yield
      byte = s_string[s_pos]
      @sio_pos = s_pos + 1
      str = ' '
      str[0] = byte
      block.call(str)
      s_pos = @sio_pos
    end
    self
  end

  alias chars each_char

  def reopen(obj=MaglevUndefined, mode=MaglevUndefined)
    # this variant gets bridge mehods
    if mode._isInteger 
      if (mode & IO::TRUNC)._not_equal?(0) && obj.frozen?
        raise TypeError, 'cannot truncate frozen input string'
      end
    else
      uu = MaglevUndefined
      if mode._equal?(uu)
        if obj._equal?(uu) 
          return self.reopen()
        else
          return self.reopen(obj)
        end
      end
    end
    self._initialize(obj, mode, true)
  end

  def reopen(other_io)
    if other_io._isString
      self._initialize(other_io, IO::RDWR, true )
    else
      other_io = Maglev::Type.coerce_to(other_io, PureRubyStringIO, :to_strio )
      self._initialize(other_io.string, other_io._mode, true)
    end
  end

  def reopen()
    self._initialize(self.string, "w+", true)
  end
end
