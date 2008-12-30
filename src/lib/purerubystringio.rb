# To load this in emergency situations when loading StringIO fails then use
# the following:
#
#   begin
#     require 'stringio'
#   rescue LoadError
#     require 'purerubystringio'    # or just put this entire file in your code at this point.
#   end
#
# If you need stringio and you know for sure it isn't going to be available
# then you can easily fake it without changing your existing code. Just add
# the following before the rest of your code starts.
#
#   class StringIO < PureRubyStringIO
#   end
#
# Any code that uses StringIO.new will now work. So will any subclass
# definitions. If you are only subclassing StringIO and you are willing to
# make small changes to your code you can change your class definitions to:
#
#   class MyClass < Object.const_defined?(:StringIO) ? StringIO : PureRubyStringIO
#     ...
#   end
#
# That will automatically select which ever is avaialable at the time. Savy
# coders, by now, will have begin to consider the delegator mixin. I'll
# leave that exercise for you to finish. ;)

class PureRubyStringIO

  include Enumerable

  SEEK_CUR = IO::SEEK_CUR
  SEEK_END = IO::SEEK_END
  SEEK_SET = IO::SEEK_SET

  @@relayMethods = [
    :<<, :all?, :any?, :binmode, :close, :close_read, :close_write, :closed?, :closed_read?,
    :closed_write?, :collect, :detect, :each, :each_byte, :each_line, :each_with_index,
    :entries, :eof, :eof?, :fcntl, :fileno, :find, :find_all, :flush, :fsync, :getc, :gets,
    :grep, :include?, :inject, :isatty, :length, :lineno, :lineno=, :map, :max, :member?,
    :min, :partition, :path, :pid, :pos, :pos=, :print, :printf, :putc, :puts, :read,
    :readchar, :readline, :readlines, :reject, :rewind, :seek, :select, :size, :sort,
    :sort_by, :string, :string=, :sync, :sync=, :sysread, :syswrite, :tell, :truncate, :tty?,
    :ungetc, :write, :zip]

  def self.open(string="", mode="r+")
    if block_given? then
      sio = new(string, mode)
      rc = yield(sio)
      sio.close
      rc
    else
      new(string, mode)
    end
  end

  def <<(obj)
    requireWritable
    write obj
    self
  end

  def binmode
    self
  end

  def close
    requireOpen
    @sio_closed_read = true
    @sio_closed_write = true
    self
  end

  def close_read
    raise IOError, "closing non-duplex IO for reading", caller if closed_read?
    @sio_closed_read = true
    self
  end

  def close_write
    raise IOError, "closing non-duplex IO for writing", caller if closed_write?
    @sio_closed_read = true
    self
  end

  def closed?
    closed_read? && closed_write?
  end

  def closed_read?
    @sio_closed_read
  end

  def closed_write?
    @sio_closed_write
  end

  def each(sep_string=$/, &block)
    requireReadable
    @sio_string.each(sep_string, &block)
    @sio_pos = @sio_string.length
  end

  def each_byte(&block)
    requireReadable
    @sio_string.each_byte(&block)
    @sio_pos = @sio_string.length
  end

  def eof
    requireReadable { @sio_pos >= @sio_string.length }
  end

  def fcntl(integer_cmd, arg)
    raise NotImplementedError, "The fcntl() function is unimplemented on this machine", caller
  end

  def fileno
    nil
  end

  def flush
    self
  end

  def fsync
    0
  end

  def getc
    requireReadable
    char = @sio_string[@sio_pos]
    @sio_pos +=  1 unless char.nil?
    char
  end

  def gets(sep_string=$/)
    requireReadable
    @sio_lineno += 1
    pstart = @sio_pos
    @sio_pos = @sio_string.index(sep_string, @sio_pos) || [@sio_string.length, @sio_pos].max
    @sio_string[pstart..@sio_pos]
  end

  def initialize(string="", mode="r+")
    @sio_string = string.to_s
    @sio_lineno = 0
    @mode = mode
    @relay = nil
    case mode.delete("b")
    when "r"
      @sio_closed_read = false
      @sio_closed_write = true
      @sio_pos = 0
    when "r+"
      @sio_closed_read = false
      @sio_closed_write = false
      @sio_pos = 0
    when "w"
      @sio_closed_read = true
      @sio_closed_write = false
      @sio_pos = 0
      @sio_string.replace("")
    when "w+"
      @sio_closed_read = false
      @sio_closed_write = false
      @sio_pos = 0
      @sio_string.replace("")
    when "a"
      @sio_closed_read = true
      @sio_closed_write = false
      @sio_pos = @sio_string.length
    when "a+"
      @sio_closed_read = false
      @sio_closed_write = false
      @sio_pos = @sio_string.length
    else
      raise ArgumentError, "illegal access mode #{mode}", caller
    end
  end

  def isatty
    flase
  end

  def length
    @sio_string.length
  end

  def lineno
    @sio_lineno
  end

  def lineno=(integer)
    @sio_lineno = integer
  end

  def path
    nil
  end

  def pid
    nil
  end

  def pos
    @sio_pos
  end

  def pos=(integer)
    raise Errno::EINVAL, "Invalid argument", caller if integer < 0
    @sio_pos = integer
  end

  def print(*args)
    requireWritable
    args.unshift($_) if args.empty
    args.each { |obj| write(obj) }
    write($\) unless $\.nil?
    nil
  end

  def printf(format_string, *args)
    requireWritable
    write format(format_string, *args)
    nil
  end

  def putc(obj)
    requireWritable
    write(obj.is_a?(Numeric) ? sprintf("%c", obj) : obj.to_s[0..0])
    obj
  end

  def puts(*args)
    requireWritable
    args.unshift("") if args.empty?
    args.each { |obj|
      write obj
      write $/
    }
    nil
  end

  def read(length=nil, buffer=nil)
    requireReadable
    len = length || [@sio_string.length - @sio_pos, 0].max
    raise ArgumentError, "negative length #{len} given", caller if len < 0
    buffer ||= ""
    pstart = @sio_pos
    @sio_pos += len
    buffer.replace(@sio_string[pstart..(@sio_pos - 1)])
#    buffer.replace(@sio_string[pstart..@sio_pos])
    buffer.empty? && !length.nil? ? nil : buffer
  end

  def readchar
    requireReadable
    raise EOFError, "End of file reached", caller if eof?
    getc
  end

  def readline
    requireReadable
    raise EOFError, "End of file reached", caller if eof?
    gets
  end

  def readlines(sep_string=$/)
    requireReadable
    raise EOFError, "End of file reached", caller if eof?
    rc = []
    until eof
      rc << gets(sep_string)
    end
    rc
  end

  def reopen(string, mode=nil)
    if string.is_a?(self.class) then
      raise ArgumentError, "wrong number of arguments (2 for 1)", caller if !mode.nil?
      @relay = string
      instance_eval(%Q{
        class << self
          @@relayMethods.each { |name|
            define_method(name, ObjectSpace._id2ref(#{@relay.object_id}).method(("original_" + name.to_s).to_sym).to_proc)
          }
        end
      })
    else
      raise ArgumentError, "wrong number of arguments (1 for 2)", caller if mode.nil?
      class << self
        @@relayMethods.each { |name|
          alias_method(name, "original_#{name}".to_sym)
          public name
        }
        @relay = nil
      end unless @relay.nil?
      @sio_string = string.to_s
      @mode = mode
    end
  end

  def rewind
    @sio_pos = 0
    @sio_lineno = 0
  end

  def seek(amount, whence=SEEK_SET)
    if whence == SEEK_CUR then
      offset += @sio_pos
    elsif whence == SEEK_END then
      offset += size
    end
    @sio_pos = offset
  end

  def string
    @sio_string
  end

  def string=(newstring)
    @sio_string = newstring
  end

  def sync
    true
  end

  def sync=(boolean)
    boolean
  end

  def sysread(length=nil, buffer=nil)
    requireReadable
    raise EOFError, "End of file reached", caller if eof?
    read(length, buffer)
  end

  def syswrite(string)
    requireWritable
    @sio_string[@sio_pos, string.length] = string
    @sio_pos +=  string.size
    string.size
  end

  # In ruby 1.8.4 truncate differs from the docs in two ways.

  # First, if an integer greater that the length is given then the string
  # is expanded to the new integer length. As this expansion seems to
  # contain junk characters instead of nulls I suspect this may be a flaw
  # in the C code which could cause a core dump if abused/used.
  #
  # Second, the documentation states that truncate returns 0. It returns
  # the integer instead.  This implementation follows the documentation in
  # the first instance as I suspect this will be fixed in the C code. In
  # the second instance, it follows the actions of the C code instead of
  # the docs.  This was decided as it causes no immedeate harm and this
  # ruby implentation is to be as compatable as possible with the C
  # version. Should the C version change to match the docs the ruby version
  # will be simple to update as well.
  def truncate(integer)
    requireWritable
    raise Errno::EINVAL, "Invalid argument - negative length", caller if integer < 0
    @sio_string[[integer, @sio_string.length].max..-1] = ""
    integer
  end

  def ungetc(integer)
    requireWritable
    if @sio_pos > 0 then
      @sio_pos -= 1
      putc(integer)
      @sio_pos -= 1
    end
  end

  alias :each_line :each
  alias :eof? :eof
  alias :size :length
  alias :tty? :isatty
  alias :tell :pos
  alias :write :syswrite

  protected
  @@relayMethods.each { |name|
    alias_method("original_#{name}".to_sym, name)
    protected "original_#{name}".to_sym
  }

  private

  def requireReadable
    raise IOError, "not opened for reading", caller[1..-1] if @sio_closed_read
  end

  def requireWritable
    raise IOError, "not opened for writing", caller[1..-1] if @sio_closed_write
  end

  def requireOpen
    raise IOError, "closed stream", caller[1..-1] if @sio_closed_read && @sio_closed_write
  end

end
