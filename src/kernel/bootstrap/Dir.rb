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

  # RUBINIUS inspired, but our API is enough different..
  def self.chdir(path = ENV['HOME'])
    if block_given?
      original_path = self.getwd
      Errno.handle(_chdir(path), "chdir #{path}")

      begin
        value = yield path
      ensure
        Errno.handle(_chdir(original_path), "chdir back to #{original_path}")
      end

      return value
    else
      Errno.handle(_chdir(path), "chdir #{path}")
    end
  end

  def self.chroot(dirname)
    raise SecurityError  # MNU, and only root could do this anyway
  end

  def self.delete(dirname)
    Errno.handle(_rmdir(dirname), "delete #{dirname}")
  end

  def self.entries(dirname)
    Dir.new(dirname).entries
  end

  def self.foreach(dirname, &block)
    Dir.entries(filename).each(&block)
  end

  def self.getwd
    Errno.handle(_getwd, "getwd")
  end

  # MNI: Dir.glob

  # if not nil, permissions must be >= 0 and <= 0777 if permissions==nil ,
  # the created directory will have permissions as specified by current
  # value of File.umask()
  def self.mkdir(dirname, permissions=nil)
    Errno.handle(_mkdir(dirname, permissions), "mkdir #{dirname}  #{permissions}")
  end

  def self.new(dirname)
    inst = _new(dirname)
    Errno.handle(inst, dirname)
    inst.initialize(dirname)
  end

  # MNI: Dir.open

  def self.pwd
    Errno.handle(getwd, "pwd")
  end

  def self.rmdir(dirname)
    Errno.handle(_rmdir(dirname), "rmdir #{dirname}")
  end

  def self.unlink(dirname)
    Errno.handle(_rmdir(dirname), "unlink #{dirname}")
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
