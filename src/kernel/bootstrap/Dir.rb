class Dir
  include Enumerable

  # private primitives
  class_primitive_nobridge '_rmdir', '_rmdir:'
  class_primitive_nobridge '_chdir', '_chdir:'
  class_primitive_nobridge '_getwd', '_getwd'
  class_primitive_nobridge '_new', '_new:'
  class_primitive_nobridge '_mkdir', '_mkdir:permissions:'

  # Class Methods

  # MNI: Dir.[]
  # MNI: Dir.entries
  # MNI: Dir.foreach
  # MNI: Dir.glob

  def self.new(dirname)
    inst = _new(dirname)
    if (inst._isSmallInteger)
      raise SystemCallError  # TODO: use inst which is errno value
    end
    inst.initialize(dirname)
  end

  def self.delete(dirname)
    status = _rmdir(dirname)
    if (status.equal?(0))
      return 0
    end
    raise SystemCallError  # TODO: use status which is errno value
  end

  def self.rmdir(dirname)
    rmdir(dirname)
  end

  def self.unlink(dirname)
    rmdir(dirname)
  end

  def self.chdir
    raise NotImplementedError # TODO , use HOME env ....
  end

  def self.chdir(aString)
    status = _chdir(aString)
    if (status.equal?(0))
      return 0
    end
    raise SystemCallError  # TODO: use status which is errno value
  end

  def self.chroot(dirname)
    raise SecurityError  # MNU, and only root could do this anyway
  end

  def self.getwd
    res = _getwd
    if (res._isSmallInteger)
      raise SystemCallError  # TODO: use res which is errno value
    end
    res
  end

  def self.pwd
    getwd
  end

  def mkdir(dirname, permissions=nil)
    # if not nil, permissions must be >= 0 and <= 0777
    # if permissions==nil ,  the created directory will have
    #   permissions as specified by current value of File.umask()
    status = _mkdir(dir, permissions)
    if (status.equal?(0))
      return 0
    end
    raise SystemCallError  # TODO: use status which is errno value
  end

  # Instance Methods
  def initialize(path)
    @path    = path
    # @entries has been filled in by _new primitive
    @index   = 0
    @closed  = false
    @range   = 0...@entries.length
    self
  end

  def close
    raise IOError, "in 'close'" if @closed
    @closed = true
  end

  def each(&block)
    check_closed

    i = 0
    lim = @entries.size
    while i < lim
      block.call(@entries[i])
      i = i + 1
    end
  end

  def entries
    check_closed
    @entries
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
