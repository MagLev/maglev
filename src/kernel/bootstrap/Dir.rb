class Dir

  # private primitives
  class_primitive_nobridge '__rmdir', '_rmdir:'
  class_primitive_nobridge '__chdir', '_chdir:'
  class_primitive_nobridge '__getwd', '_getwd'
  class_primitive_nobridge '__new', '_new:'
  class_primitive_nobridge '__mkdir', '_mkdir:permissions:'
  class_primitive_nobridge '__get_clear_errno', '_errno'  # gets and clears errno

  class_primitive_nobridge '__getgrgent', '_getgrgent:'
  class_primitive_nobridge '__getgrgid', '_getgrgid:'
  class_primitive_nobridge '__getgrnam', '_getgrnam:'
  class_primitive_nobridge '__getlogin', '_getlogin'
  class_primitive_nobridge '__getpwent', '_getpwent:'
  class_primitive_nobridge '__getpwnam', '_getpwnam:'
  class_primitive_nobridge '__getpwuid', '_getpwuid:'
  class_primitive_nobridge '__getuid',   '_getuid'

  # Class Methods

  # RUBINIUS inspired, but our API is enough different..
  def self.chdir(path = ENV['HOME'])
    path = Type.coerce_to(path, String, :to_str)
    if block_given?
      original_path = self.getwd
      Errno.handle(__chdir(path), "chdir #{path}")

      begin
        value = yield path
      ensure
        Errno.handle(__chdir(original_path), "chdir back to #{original_path}")
      end

      return value
    else
      Errno.handle(__chdir(path), "chdir #{path}")
    end
  end

  def self.chroot(dirname)
    raise SecurityError  # MNU, and only root could do this anyway
  end

  def self.delete(dirname)
    Errno.handle(__rmdir(dirname), "delete #{dirname}")
  end

  def self.entries(dirname)
    Dir.new(dirname).entries
  end

  def self.foreach(dirname, &block)
    Dir.entries(dirname).each(&block)
    nil
  end

  def self.getwd
    Errno.handle(__getwd, "getwd")
  end

  # if not nil, permissions must be >= 0 and <= 0777 if permissions==nil ,
  # the created directory will have permissions as specified by current
  # value of File.umask()
  def self.mkdir(dirname, permissions=0777)
    # MRI does not allow conversion of nil to 0 for this method...
    raise TypeError, "no implicit conversion from nil to integer" if permissions._equal?(nil)
    permissions = Type.coerce_to(permissions, Integer, :to_i)
    Errno.handle(__mkdir(dirname, permissions), "mkdir #{dirname}  #{permissions}")
  end

  def self.new(*args, &blk)
    # first variant gets bridge methods
    if (args.length > 0)
      d = __new(args[0])
      Errno.handle(d, dirname)
      d.initialize(*args, &blk)
    else
      raise ArgumentError, 'too few args'
    end
  end

  def self.new(dirname)
    # replaces the corresponding bridge method only
    d = __new(dirname)
    Errno.handle(d, dirname)
    d.initialize(dirname)
    d
  end

  def self.open(dirname, &block)

    if block_given?
      begin
        d = Dir.new dirname
        result = yield d
      ensure
        d.close unless d._equal?(nil)
      end
      result
    else
      Dir.new dirname
    end
  end

  def self.pwd
    Errno.handle(getwd, "pwd")
  end

  def self.rmdir(dirname)
    Errno.handle(__rmdir(dirname), "rmdir #{dirname}")
  end

  def self.unlink(dirname)
    Errno.handle(__rmdir(dirname), "unlink #{dirname}")
  end


  # Instance Methods

  def initialize(path)
    @path    = path
    # @entries has been filled in by __new primitive
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
    @index
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
    self
  end

  def seek(pos)
    check_closed
    p = Type.coerce_to(pos, Fixnum, :to_i)

    @index = p if (0...@entries.size) === p
    self 
  end

  alias tell pos

  def check_closed
    raise IOError, "closed directory" if @closed
  end
end
