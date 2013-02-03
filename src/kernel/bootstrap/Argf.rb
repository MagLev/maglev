# modified Rubinius code  , orginal copy from Rubinius on April 5, 2010

module Maglev
  class Argf    # identical to smalltalk RubyArgf,   resolved in Maglev1.rb
    include Enumerable

    # def initialize ; end  # in Smalltalk

    #
    # Bytewise iterator.
    #
    # @see  #each_byte
    def bytes()  # added for 1.8.7
      return IoByteEnumerator.new(self, :each_byte)
    end

    def bytes(&b)
      unless block_given?
        return IoByteEnumerator.new(self, :each_byte) # for 1.8.7
      end
      self.each_byte(&b)
      self
    end

    #
    # Set stream into binary mode.
    #
    # Stream is set into binary mode, i.e. 8-bit ASCII.
    # Once set, the binary mode cannot be undone. Returns
    # self.
    # No effect in 1.8.7
    def binmode
      self
    end

    #
    # Character iterator.
    #
    # @see  #each_char
    #
    def chars(&b)
      unless block_given?
        return IoCharEnumerator.new(self, :each_char)
      end 
      self.each_char(&b)
      self
    end

    #
    # Close current stream.
    def close
      stream = @_st_stream
      if stream._equal?(File.__stdin)
        # do nothing
      elsif stream._not_equal?(nil) && ! stream.closed?
        stream.close
        @_st_advance = true 
        @_st_lineno = 0
        @_st_stream = nil
      end
      self
    end

    #
    # True if the stream is closed.
    def closed?
      stream = @_st_stream
      if stream._equal?(nil)
        true
      else
        stream.closed?
      end
    end

    #
    # Linewise iteration.
    #
    # Yields one line from stream at a time, as given by
    # #gets. An Enumerator is returned if no block is
    # provided. Returns nil if no content, self otherwise.
    #
    # @see  #gets.
    #
    def each_line(sep=$/, &block)
      unless block_given?
        return IoEnumerator.new(self, :each_line, sep)  # for 1.8.7
      end
      if sep._equal?(nil)
        block.call( self.__contents )
      else
        sep = Maglev::Type.coerce_to(sep, String, :to_str)
        if sep.__size._equal?(0)
          while true
            para = self.__next_paragraph
            if para._equal?(nil)
              return self
            end
            block.call(para)
          end
        else
          while true
            line = self.__next_line( sep)
            if line._equal?(nil)
              return self
            end
            block.call( line )
          end
        end
      end
      self
    end

    #
    # Linewise iteration.
    #
    # @see  #each_line.
    #
    alias each each_line

    #
    # Bytewise iteration.
    #
    # Yields one byte at a time from stream, an Integer
    # as given by #getc. An Enumerator is returned if no
    # block is provided. Returns self.
    #
    # @see  #getc
    #
    def each_byte(&block)
      unless block_given?
        return IoByteEnumerator.new(self, :each_byte) # for 1.8.7
      end
      while ch = getbyte()
        yield ch
      end
      self
      self
    end 


    #
    # Character-wise iteration.
    #
    # Yields one character at a time from stream. An
    # Enumerator is returned if no block is provided.
    # Returns self.
    #
    # The characters yielded are gotten from #getc.
    #
    # @see  #getc
    #
    def each_char(&block)
      unless block_given?
        return IoEnumerator.new(self, :each_char)  # for 1.8.7
      end
      while c = getbyte()
        str = ' '
        str[0] = c 
        yield str
      end
      self
    end

    #
    # Query whether stream is at end-of-file.
    #
    # True if there is a stream and it is in EOF
    # status.
    #
    def eof?
      stream = @_st_stream
      stream and stream.eof?
    end
    alias_method :eof, :eof?

    #
    # File descriptor number for stream.
    #
    # Returns a file descriptor number for the stream being
    # read out of.
    #
    # @todo   Check correctness, does this imply there may be
    #         multiple FDs and if so, is this correct? --rue
    #
    def fileno
      raise ArgumentError, "No stream" unless __advance!
      @_st_stream.fileno
    end
    alias_method :to_i, :fileno

    #
    # File path currently in use.
    #
    # Path to file from which read currently is
    # occurring, or an indication that the stream
    # is STDIN.
    #
    def filename
      __advance!
      @_st_fileName
    end
    alias_method :path, :filename

    #
    # Current stream object.
    #
    # This may change during the course of execution,
    # but is the current one!
    #
    def file
      __advance!
      @_st_stream
    end

    #
    # Return one character from stream.
    #
    # If a character cannot be returned and we are
    # reading from a file, the stream is closed.
    #
    def getbyte
      while true          # Performance
        return nil unless __advance!
        stream = @_st_stream
        if val = stream.getc
          return val
        end

        return nil if stream._equal?(File.__stdin)
        stream.close unless stream.closed?
        @_st_advance = true
      end
    end
    alias_method :getc, :getbyte

    #
    # Return next line of text from stream.
    #
    # If a line cannot be returned and we are
    # reading from a file, the stream is closed.
    #
    # The mechanism does track the line numbers,
    # and updates $. accordingly.
    #
    def gets(*args)    # [  begin gets implementation
      raise ArgumentError, 'expected 0 or 1 arg with no block'
    end
  
    def gets(sep)
      self.__gets(sep, 0x31)
    end

    def gets
      self.__gets($/, 0x31)
    end

    def __gets(a_sep, vcGlobalIdx)
      # __gets maybe reimplemented in subclasses
      if a_sep._equal?(nil)
        res = self.__contents
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

    def __increment_lineno
      num = @_st_lineno + 1
      $. = num
      @_st_lineno = num
      num
    end

    def __next_line(sep)
      while true          
        unless __advance!
          return nil
        end
        stream = @_st_stream
        line = stream.__next_line_to(sep)
        unless line
          return nil if stream._equal?(File.__stdin)
          stream.close unless stream.closed?
          @_st_advance = true
          next
        end
        self.__increment_lineno
        return line
      end
    end

    def __contents
      str = ''
      while true          
        return str unless __advance!
        stream = @_st_stream
        str << stream.__contents
        return str if stream._equal?(File.__stdin)
        stream.close unless stream.closed?
        @_st_advance = true
      end
    end

    def __next_paragraph
      while true
        return nil unless __advance!
        stream = @_st_stream
        para = stream.__next_paragraph
        unless para
          return nil if stream._equal?(File.__stdin)
          stream.close unless stream.closed?
          @_st_advance = true
          next
        end
        return para
      end
    end

    #
    # Returns Enumerator for linewise iteration.
    #
    # Does not iterate directly, returns an Enumerator.
    # If iteration is desired, use #each_line.
    #
    # @see  #each_line.
    #
    def lines(sep=$/)  # added for 1.8.7
      return IoEnumerator.new(self, :each_line, sep)
    end

    #
    # Return current line number.
    #
    # Line numbers are maintained when using the linewise
    # access methods.
    #
    # @see  #gets
    # @see  #each_line
    #
    def lineno
      @_st_lineno
    end

    #
    # Set current line number.
    #
    # Also sets $. accordingly.
    #
    # @todo Should this be public? --rue
    #
    def lineno=(num)
      num = Maglev::Type.coerce_to(num, Fixnum, :to_int)
      @_st_lineno = num
      $. = num
      num
    end

    #
    # Return stream position for seeking etc.
    #
    # @see IO#pos.
    #
    def pos
      raise ArgumentError, "no stream" unless __advance!
      @_st_stream.tell
    end
    alias_method :tell, :pos

    #
    # Set stream position to a previously obtained position.
    #
    # @see IO#pos=
    #
    def pos=(position)
      raise ArgumentError, "no stream" unless __advance!
      @_st_stream.pos = position
    end

    #
    # Read a byte from stream.
    #
    # Similar to #getc, but raises an EOFError if
    # EOF has been reached.
    #
    # @see  #getc
    #
    def readbyte
      __advance!
      if val = getbyte
        return val
      end

      raise EOFError, "ARGF at end"
    end
    alias_method :readchar, :readbyte

    #
    # Read number of bytes or all, optionally into buffer.
    #
    # If number of bytes is not given or is nil, tries to read
    # all of the stream, which is then closed. If the number is
    # specified, then at most that many bytes will be read.
    #
    # A buffer responding to #<< may be provided as the second
    # argument. The data read is pushed into it. If no buffer
    # is provided, as by default, a String with the data is
    # returned instead.
    #
    def read(count = nil, output = nil)
      # The user might try to pass in nil, so we have to check here
      output ||= ""

      if count
        bytes_left = count

        until bytes_left == 0
          return output unless __advance!

          stream = @_st_stream
          if res = stream.read(bytes_left)
            output << res
            bytes_left -= res.__size
          else
            break if stream._equal?(File.__stdin)
            stream.close unless stream.closed?
            @_st_advance = true
          end

        end

        return output
      end

      while __advance!
        stream = @_st_stream
        output << stream.read

        break if stream._equal?(File.__stdin)
        stream.close unless stream.closed?
        @_st_advance = true
      end

      output
    end

    #
    # Read all lines from stream.
    #
    # Reads all lines into an Array using #gets and
    # returns the Array.
    #
    # @see  #gets
    #
    def readlines
      return nil unless __advance!

      lines = []
      while line = gets()
        lines << line
      end

      lines
    end
    alias_method :to_a, :readlines

    #
    # Read next line of text.
    #
    # As #gets, but an EOFError is raised if the stream
    # is at EOF.
    #
    # @see  #gets
    #
    def readline
      line = self.__gets($/, 0x31)
      if line._equal?(nil)
        raise EOFError, "ARGF at end"
      end
      line
    end

    #
    # Rewind the stream to its beginning.
    #
    # Line number is updated accordingly.
    #
    # @todo Is this correct, only current stream is rewound? --rue
    #
    def rewind
      raise ArgumentError, "no stream to rewind" unless __advance!
      @_st_stream.rewind
      @_st_lineno = 0
    end

    # during bootstrap,  send and __send__ get no bridge methods
    def send(sym)
      if (sym._equal?(:gets))
	return __gets($/ , 0x31)
      end
      if (sym._equal?(:readline))
        line = __gets($/ , 0x31)
        if line._equal?(nil)
          raise EOFError, "ARGF at end"
        end
        return line
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
      if (sym._equal?(:readline))
        line = __gets($/ , 0x31)
        if line._equal?(nil)
          raise EOFError, "ARGF at end"
        end
        return line
      end
      super(sym)
    end

    def __send__(sym, arg)
      if (sym._equal?(:gets))
	return __gets(arg , 0x31)
      end
      super(sym, arg)
    end

    #
    # Seek into a previous position in the stream.
    #
    # @see IO#seek.
    #
    def seek(*args)
      raise ArgumentError, "no stream" unless __advance!
      @_st_stream.seek(*args)
    end

    #
    # Close file stream and return self.
    #
    # STDIN is not closed if being used, otherwise the
    # stream gets closed. Returns self.
    #
    # @todo Need more detail on the genesis/purpose
    #       of this method. --rue
    def skip
      stream = @_st_stream
      if stream._equal?(File.__stdin)
        return self
      end
      stream.close unless stream.closed?
      @_st_advance = true
      self
    end

    #
    # Return IO object for current stream.
    #
    # @see IO#to_io
    #
    def to_io
      __advance!
      @_st_stream.to_io
    end

    #
    # Returns "ARGF" as the string representation of this object.
    #
    def to_s
      "ARGF"
    end


    # Internals
    #
    # If not initialised yet, sets the object up on either
    # first of provided file names or STDIN.
    #
    # Does nothing further or later if using STDIN, but if
    # there are further file names in ARGV, tries to open
    # the next one as the current stream.
    #
    def __advance!
      return true unless @_st_advance

      argv = @_st_argv
      av_siz = argv.__size
      stream = @_st_stream
      if av_siz._equal?(0) && stream._equal?(nil)
        @_st_advance = false
        @_st_stream = File.__stdin
        @_st_fileName = "-"
        return true
      end

      if stream._equal?(File.__stdin) || argv.__size._equal?(0)
        return false
      end
      @_st_advance = false
      fname = argv.__shift
      @_st_stream = (fname == "-" ? File.__stdin : File.open(fname, "r"))
      @_st_fileName = fname
      return true
    end
    private :__advance!

  end
end
