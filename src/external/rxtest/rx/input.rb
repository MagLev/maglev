module RX

  class Input

    @@lt = 0x3c
    @@question = 0x3f
    @@UTF8 = :"UTF-8"
    @@ISO8859 = :"ISO-8859"
    @@UTF16LE = :"UTF-16LE"
    @@UTF16BE = :"UTF-16BE"
    @@ASCII = :ASCII
    @@LIKEASCII = :"LIKE-ASCII"
    @@HIGH_SURROGATE_BASE = 0xD800
    @@HIGH_SURROGATE_MAX = 0xDBFF
    @@LOW_SURROGATE_BASE = 0xDC00
    @@LOW_SURROGATE_MAX = 0xDFFF
    @@SURROGATE_OFFSET = 0x10000 - (0xD800 << 10) - 0xDC00

    attr_reader :encoding, :lines, :chars

    def initialize(source)
      if source.kind_of? String
        source = StringSource.new(source)
      elsif source.kind_of? IO
        source = IOSource.new(source)
      elsif (source.respond_to? :next_byte) &&
          (source.respond_to? :read_some)
        # we're OK
      else
        raise(ArgumentError,
              "Source must be a String, IO, or respond to next_byte & read_some")
      end
      @source = source
      @char_buf = []
      @bufs_waiting = []
      @char_index = 0
      @encoding = find_encoding
      @chars = 0
      @lines = 1
    end

    def next_chars
      if @char_index < @char_buf.length
        s = @char_buf[@char_index .. -1]
      else
#        t1 = Time.now.to_f
        next_buf
 #       $s += (Time.now.to_f - t1)
        s = @char_buf
      end
      if @char_buf
        @char_index = @char_buf.length
      end
      s
    end

    def next_buf
      @char_buf = nil
      @char_index = 0

      if !@bufs_waiting.empty?
        @char_buf = @bufs_waiting.shift
        @char_index = 0
        return
      end

      buf = @source.read_some
      return unless buf

      case @encoding
      when @@UTF8
        while (buf[-1] & 0x80 == 0x80)
          # argh, stopped reading in the middle of a character
          grow_buf buf
        end
        @bufs_waiting = buf.scan /<|&|[^<&]+/
        @bufs_waiting.map! { |b| b.unpack('U*') }
        
      when @@ISO8859, @@LIKEASCII, @@ASCII
        @bufs_waiting = buf.scan /<|&|[^<&]+/m
        @bufs_waiting.map! { |b| b.unpack('U*') }

      when @@UTF16LE, @@UTF16BE
        grow_buf(buf) if (buf.length % 2) != 0
        last = (buf[-1] << 8) | buf[-2]
        if surrogate?(last)
          2.times { grow_buf(buf) }
        end
        @bufs_waiting.clear
        this_buf = []
        i = 0
        while i < buf.length do
          c = next_utf16_char(buf, i)
          i += (c < 0x10000) ? 2 : 4
          if c == 0x26 || c == 0x3c
            @bufs_waiting << this_buf if !this_buf.empty?
            @bufs_waiting << [ c ]
            this_buf = []
          else
            this_buf << c
          end
        end

        @bufs_waiting << this_buf if !this_buf.empty?
      
      else
        raise(ArgumentError, "@encoding botch")
      end

      next_buf
    end

    def grow_buf buf
      n = @source.next_byte
      if n == nil
        raise(SyntaxError,
              "Input source ends in the middle of a #{@encoding} character")
      end
      buf = buf + [ n ].pack('C')
    end

    def encoding= requested
      # if not declared
      if requested == nil
        if @encoding == @@LIKEASCII
          @encoding = @@UTF8
        end

      # it was declared; consistency check
      else
        case requested
        when /utf-?8/i
          if @encoding == @@UTF8
            # cool
          elsif @encoding == @@LIKEASCII
            @encoding = @@UTF8
          else
            raise(SyntaxError,
                  "Declared Encoding '#{requested}' incompatible with input")
          end
        when /iso-?8859-1/i
          if @encoding == @@LIKEASCII
            @encoding = @@ISO8859
          else
            raise(SyntaxError,
                  "Declared Encoding '#{requested}' incompatible with input")
          end
        when /ascii/i
          if @encoding == @@LIKEASCII
            @encoding = @ASCII
          else
            raise(SyntaxError,
                  "Declared Encoding '#{requested}' incompatible with input")
          end
        else
          raise(SyntaxError,
                "Declared Encoding '#{requested}' is not supported")
        end
      end
    end

    def utf16_char b1, b2
      if @encoding == @@UTF16BE
        (b1 << 8) | b2
      else
        (b2 << 8) | b1
      end
    end

    def surrogate? c
      c >= @@HIGH_SURROGATE_BASE && c <= @@LOW_SURROGATE_MAX
    end
    
    # NB relies on @encoding
    def next_utf16_char buf, at
      c0 = utf16_char(buf[at], buf[at + 1])
      return c0 unless surrogate?(c0)
      c1 = next_utf16_char(buf, at + 2)
      return combine_surrogates(c0, c1)
    end

    def combine_surrogates(s_hi, s_lo)
      lead = (s_hi << 10) + s_lo + @@SURROGATE_OFFSET
    end

    def first_utf8
      b = @source.next_byte
      if    b == nil         then return 0, nil
      elsif b & 0x80 == 0    then return 1, b
      elsif b & 0xe0 == 0xc0 then return 2, b & 0x1f
      elsif b & 0xf0 == 0xe0 then return 3, b & 0x0f
      else                        return 4, b & 0x07
      end
    end
    
    def next_6bits
      @source.next_byte & 0x3f
    end

    def next_utf8_char
      len, c = first_utf8
      case len
      when 2
        c = (c << 6) | next_6bits
      when 3
        c = (c << 12) | (next_6bits << 6) | next_6bits
      when 4
        c = (c << 18) | (next_6bits << 12) | (next_6bits << 6) | next_6bits
      end
      return c
    end

    def find_encoding
      begin
        b0 = @source.next_byte
        b1 = @source.next_byte
        raise IOError("Can't read 2 bytes") unless b0 && b1
      rescue Exception
        raise(IOError, $!)
      end
      
      if b0 == @@lt
        save b0, b1

        # '<', ASCII-like
        if (b1 == @@question)

          # <? an XML declaration
          return @@LIKEASCII

          # < without ?, gotta be UTF8
        else
          return @@UTF8
        end

      # check for a BOM
      elsif b0 == 0xfe && b1 == 0xff
        return @@UTF16BE

      elsif b0 == 0xff && b1 == 0xfe
        return @@UTF16LE

      else
        # doesn't start with '<' or a BOM
        save b0, b1
        return @@UTF8
      end
    end

    def save *a
      @char_buf.insert(@char_index, *a)
    end
  end

  class StringSource
    def initialize(source)
      @s = source
      @ind = 0
    end

    def read_some
      s = @s[@ind .. -1]
      @ind = @s.size
      s
    end
    
    def next_byte
      c = @s[@ind]
      @ind += 1
      return c
    end
  end

  class IOSource
    def initialize(source, buf_size = 65536)
      @s = source
      @buf = []
      @buf_ind = 0
      @buf_size = buf_size
    end

    def read_some
      if @buf_ind == @buf.size
        @s.read @buf_size
      else
        s = @buf[@buf_ind .. -1]
        @buf_ind = @buf.size
        s
      end
    end

    def next_byte
      if @buf_ind < @buf.size
        c = @buf[@buf_ind]
        @buf_ind += 1
        c
      else
        @buf = @s.read @buf_size
        @buf_ind = 0
        if @buf == nil
          nil
        else
          next_byte
        end
      end
    end
  end

end
