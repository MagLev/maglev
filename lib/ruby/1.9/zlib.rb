require 'stringio'

module Zlib

  # The superclass for all exceptions raised by Ruby/zlib.
  class Error < StandardError; end

  # constants from zlib.h as linked into Gemstone libgcilnk.so
  ZLIB_VERSION = "1.2.5"

  NO_FLUSH    =  0
  # PARTIAL_FLUSH = 1 # will be removed, use SYNC_FLUSH instead
  SYNC_FLUSH    = 2
  FULL_FLUSH    = 3  # inflate() will terminate a block of compressed data
  FINISH        = 4
  BLOCK         = 5  # deflate() will return at end of next block boundary,
                        #  or after the gzip header .
  TREES         = 6

  OK            = 0
  STREAM_END    = 1
  NEED_DICT     = 2
  ERRNO        = (-1)
  STREAM_ERROR = (-2)
  DATA_ERROR   = (-3)
  MEM_ERROR    = (-4)
  BUF_ERROR    = (-5)
  VERSION_ERROR = (-6)

  NO_COMPRESSION     =    0
  BEST_SPEED         =    1
  BEST_COMPRESSION   =    9
  DEFAULT_COMPRESSION = (-1)

  FILTERED           = 1
  HUFFMAN_ONLY       = 2
  RLE                = 3
  FIXED              = 4
  DEFAULT_STRATEGY   = 0

  DEFLATED   = 8

  # from zconf.h
  MAX_WBITS = 15 # 32K LZ77 window
  MAX_MEM_LEVEL = 9
  DEF_MEM_LEVEL = 9

  OS_UNIX     = 0x03
  OS_CODE = OS_UNIX

  # class Zlib::ZStream was defined during bootstrap in zlib_czstream.rb

  class GzipFile  # {

    class Error < Zlib::Error; end

    #SYNC            = Zlib::ZStream::UNUSED
    #HEADER_FINISHED = Zlib::ZStream::UNUSED << 1
    #FOOTER_FINISHED = Zlib::ZStream::UNUSED << 2

    FLAG_MULTIPART    = 0x2
    FLAG_EXTRA        = 0x4
    FLAG_ORIG_NAME    = 0x8
    FLAG_COMMENT      = 0x10
    FLAG_ENCRYPT      = 0x20
    FLAG_UNKNOWN_MASK = 0xc0

    EXTRAFLAG_FAST    = 0x4
    EXTRAFLAG_SLOW    = 0x2

    MAGIC1         = 0x1f
    MAGIC2         = 0x8b
    METHOD_DEFLATE = 8

    def self.wrap(io, &block)
      obj = self.new(io)
      if block_given? then
        begin
          yield obj
        ensure
          obj.close
        end
      end
    end

    # TODO: protect against multiple calls to close, or a close after a finish
    def close
      io = finish()
      io.close if io.respond_to? :close
      io
    end

    def closed?
      @io._equal?(nil)
    end

    def comment
      raise Error, 'closed gzip stream' if @io.nil?
      @comment.dup
    end

    # def finish # subclass responsibility

    def finished?
      self.closed?()
    end

    # def footer_finished? # not implemented
    # def header_finished? # not implemented

    def mtime
      Time.at(@mtime)
    end

    def orig_name
      raise Error, 'closed gzip stream' if @io.nil?
      @orig_name.dup
    end

  end # }


  class GzipReader < GzipFile # {

    def initialize(io, *args)
      # TODO: update it to ruby 1.9
      @io = io
      @zstream = ZStream.open_read(io, Error) # Error resolves to GzipFile::Error
      super()
      arr = @zstream.read_header()
      @orig_name = arr[0]
      @comment = arr[1]
      @mtime = arr[2]
    end

    def rewind
      @zstream.close
      @io.rewind
      @zstream = ZStream.open_read(@io, Error)
      arr = @zstream.read_header()
      @orig_name = arr[0]
      @comment = arr[1]
      @mtime = arr[2]
      0
    end

    def eof?
      @zstream.at_eof
    end

    def pos
      sio = @contents_sio
      if sio._not_equal?(nil)
        sio.pos
      else
        @zstream.total_out()
      end
    end

    def read(length = nil)
      if length.nil?
        buf = ''
        while ! eof?
          buf << @zstream.read(16384)
        end
        buf
      else
        @zstream.read(length)
      end
    end

    def finish
      # does not call close method of the associated IO object.
      @zstream.close
      io = @io
      @io = nil
      io
    end

    def each_byte(&block)
      sio = self.__contents_sio
      sio.each_byte(&block)
      self
    end

    def __contents_sio
      sio = @contents_sio
      if sio._equal?(nil)
        str = String.new
        while not eof?
          buf = self.read(4096)
          if buf._equal?(nil)
            return str
          end
          str << buf
        end
        sio = PureRubyStringIO.open(str)
        @contents_sio = sio
      end
      sio
    end

    def each_line(sep=$/, &block)
      sio = self.__contents_sio
      sio.each_line(sep, &block)
    end

    alias each each_line

    def getc
      sio = self.__contents_sio
      sio.getc
    end

    def gets(sep_string=nil)
      sio = self.__contents_sio
      if sep_string._equal?(nil)
        sio.__gets( $/ , 0x41)  # called via bridge meth
      else
        sio.__gets(sep_string, 0x31)
      end
    end

  end # }

  class GzipWriter < GzipFile  # {

    GzfError = GzipFile::Error

    ##
    # Creates a GzipWriter object associated with +io+. +level+ and +strategy+
    # should be the same as the arguments of Zlib::Deflate.new.  The
    # GzipWriter object writes gzipped data to +io+.  At least, +io+ must
    # respond to the +write+ method that behaves same as write method in IO
    # class.

    def initialize(io, level = DEFAULT_COMPRESSION,
                   strategy = DEFAULT_STRATEGY)
      @level = level
      @strategy = strategy
      @io = io
      @mtime = 0
      @buffer = ''
      @bufsize = 0
      @zstream = nil
      super()
    end

    def comment=(v)
      # Set the comment
      unless @zstream._equal?(nil) then
        raise GzfError, 'header is already written' # Error resolves to GzipFile::Error
      end
      @comment = v
    end

    def mtime=(time)
      unless @zstream._equal?(nil) then
        raise GzfError, 'header is already written' # Error resolves to GzipFile::Error
      end
      t = Integer(time)
      @mtime = t
    end

    def orig_name=(v)
      # Set the original name
      unless @zstream._equal?(nil) then
        raise GzfError, 'header is already written' # Error resolves to GzipFile::Error
      end
      @orig_name = v
    end

    def __validate_string(obj)
      unless obj._equal?(nil)
        unless obj._isString
          raise ArgumentError, 'argument must be a String'
        end
      end
    end

    def write_header
      __validate_string(@orig_name)
      __validate_string(@comment)
      lev = @level
      if (lev < DEFAULT_COMPRESSION || lev > BEST_COMPRESSION)
        raise ArgumentError, 'compression level out of range'
      end
      @zstream = ZStream.open_write(@io, Error, lev) # Error resolves to GzipFile::Error
      tim = @mtime.to_i
      unless tim._isFixnum
        raise ArgumentError, 'mtime must be a Fixnum'
      end
      arr = [ @orig_name, @comment, tim]
      @zstream.write_header(arr)
    end

    def write(data)
      strm = @zstream
      if strm._equal?(nil)
        write_header
        strm = @zstream
      end
      unless data._isString
        data = String(data)
      end
      ds = data.length
      bs = @bufsize
      if (ds >= 1024)
        if bs > 0
          strm.write(@buffer, bs)
          @bufsize = 0
        end
        strm.write(data, ds)
      else
        if (bs >= 1024)
          strm.write(@buffer, bs)
          @bufsize = 0
          bs = 0
        end
        @buffer[bs, ds] = data
        @bufsize = bs + ds
      end
    end

    alias << write

    def finish(flags=SYNC_FLUSH)
      strm = @zstream
      if strm._equal?(nil)
        write_header
        strm = @zstream
      end
      bs = @bufsize
      if bs > 0
        strm.write(@buffer, bs)
        @bufsize = 0
      end
      unless flags == SYNC_FLUSH
        strm.flush(flags)  # writes footer
      end
      @io
    end

    def flush(flags=SYNC_FLUSH)
      self.finish(flags)
    end

    def close
      self.finish(FINISH)
      @zstream.close
      @zstream = nil
      io = @io
      if io
        # RubyGems RestrictedStream only understands #initialize and #write
        io.flush if io.respond_to?(:flush)
        io.close if io.respond_to?(:close)
        @io = nil
      end
      io
    end

  end # }


  # Zlib:Inflate is the class for decompressing compressed data.  Unlike
  # Zlib::Deflate, an instance of this class is not able to duplicate
  # (clone, dup) itself.
  class Inflate < ZStream
    # Decompress +string+.
    def self.inflate(string)
      inflate_stream = Zlib::Inflate.new
      buf = inflate_stream.inflate(string)
      inflate_stream.finish
      inflate_stream.close
      buf
    end

    def inflate(string)
      __open(false, StringIO.new(string), Error, DEFAULT_COMPRESSION) # Error resolves to Zlib::Error
      self.flush_next_out
    end

    def <<(string)
      io = @ioObj
      if io._equal?(nil)
        __open(false, StringIO.new(string), Error, DEFAULT_COMPRESSION)
      else
        @ioObj << string
      end
    end

    def flush_next_out
      buf = ''
      while ! at_eof
        buf << read(2048)
      end
      buf
    end

    def finish
      flush_next_out
    end

    def close
      unless @isClosed
        @isClosed = true
        super()
      end
    end

    def closed?
      @isClosed
    end

    def finished?
      at_eof
    end
  end

  class Deflate < ZStream
    # Decompress +string+.
    #
    def self.deflate(string, level=DEFAULT_COMPRESSION)
      deflate_stream = Zlib::Deflate.new(level)
      buf = deflate_stream.deflate(string)
      deflate_stream.finish
      deflate_stream.close
      buf
    end

    # @param [Fixnum] level One of the compression levels, NO_COMPRESSION,
    #        BEST_SPEED, BEST_COMPRESSION or DEFAULT_COMPRESSION
    #
    #def initialize(level=DEFAULT_COMPRESSION, wbits=nil, memlevel=nil, strategy=nil)
    def initialize(level=DEFAULT_COMPRESSION)
      @level = level
    end

    def deflate(string, level=nil)
      compressed_data = StringIO.new('')
      __open(true, compressed_data, Error, level || @level)
      write(string, string.length)
      flush(SYNC_FLUSH)
      compressed_data.string
    end

    # This method is equivalent to <tt>deflate('', flush)</tt>.  If flush
    # is omitted, <tt>Zlib::SYNC_FLUSH</tt> is used as flush.  This method
    # is just provided to improve the readability of your Ruby program.
    def flush(flush=SYNC_FLUSH)
      # deflate('', flush) unless flush == NO_FLUSH
    end

    def finish(type=FINISH)
      # ??
    end
  end
end
