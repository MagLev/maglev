class Dir
  include Enumerable

  # Class Methods

  # MNI: Dir.[]
  # MNI: Dir.chdir
  # MNI: Dir.chroot
  # MNI: Dir.delete
  # MNI: Dir.entries
  # MNI: Dir.foreach
  # MNI: Dir.getwd
  # MNI: Dir.glob
  # MNI: Dir.mkdir
  # MNI: Dir.new
  # MNI: Dir.open

  def self.pwd
    `pwd`  # TODO: Dir.pwd: Hack to get things to compile. FIX
  end

  # MNI: Dir.rmdir
  # MNI: Dir.unlink

  # Instance Methods
  def initialize(path)
    @path    = path
    # _dir_contents returns a listing with the full path, we want just the
    # basename.  Also, _dir_contents will use the physical path, not the
    # logical, e.g., File._dir_contents('/tmp'), on a Mac, will return
    # ['/private/tmp/.',...].  So we use basename to clean up everything
    @entries = File._dir_contents(@path, false).collect { |f| File.basename f }
    @index   = 0
    @closed  = false
    @range   = 0...@entries.length
  end

  def close
    raise IOError, "in 'close'" if @closed
    @closed = true
  end

  def each(&block)
    check_closed

    i = 0
    while i < @entries.size
      block.call(@entries[i])
    end
  end

  def path
    check_closed
    @path
  end

  def pos
    check_closed
    @index
  end

  def pos=(pos)
    check_closed
    p = Type.coerce_to(pos, Fixnum, :to_i)

    @index = p if @range === p
    @index # seek returns self, pos= returns @index
  end

  def read
    check_closed

    return nil unless @range === @index

    result = @entries[@index]
    @index += 1
    result
  end

  def rewind
    check_closed
    @index = 0
  end

  def seek(pos)
    check_closed
    p = Type.coerce_to(pos, Fixnum, :to_i)

    @index = p if (0...@entries.size) === p
    self  # seek returns self, pos= returns @index
  end

  alias tell pos

  def check_closed
    raise IOError, "closed directory" if @closed
  end
end
