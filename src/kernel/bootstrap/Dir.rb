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
    path = Maglev::Type.__coerce_to_path(path)
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
    dirname = Maglev::Type.coerce_to(dirname, String, :to_str)
    Errno.handle(__rmdir(dirname), "delete #{dirname}")
  end

  def self.entries(dirname)
    Dir.new(dirname).entries
  end

  def self.foreach(dirname, &block)  # changed for 1.8.7
    if block_given?
      Dir.entries(dirname).each(&block)
      nil
    else
      # return an Enumerator
      Dir.new(dirname).each()
    end
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
    permissions = Maglev::Type.coerce_to(permissions, Integer, :to_i)
    Errno.handle(__mkdir(dirname, permissions), "mkdir #{dirname}  #{permissions}")
  end

  def self.new(*args, &block)
    # first variant gets bridge methods
    if (args.length > 0)
      d = __new(args[0])
      Errno.handle(d, dirname)
      d.initialize(*args, &block)
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
    dirname = Maglev::Type.coerce_to(dirname, String, :to_str) # 1.9 would use to_path
    if block_given?
      begin
        d = Dir.new(dirname)
        result = block.call(d)
      ensure
        d.close unless d._equal?(nil)
      end
      result
    else
      Dir.new(dirname)
    end
  end

  def self.pwd
    Errno.handle(getwd, "pwd")
  end

  def self.rmdir(dirname)
    dirname = Maglev::Type.coerce_to(dirname, String, :to_str)
    Errno.handle(__rmdir(dirname), "rmdir #{dirname}")
  end

  def self.unlink(dirname)
    dirname = Maglev::Type.coerce_to(dirname, String, :to_str)
    Errno.handle(__rmdir(dirname), "unlink #{dirname}")
  end


  # Instance Methods

  def initialize(path)
    @_st_path    = path
    # @entries has been filled in by __new primitive
    @_st_index   = 0
    @_st_closed  = false
    @_st_range   = 0...@_st_entries.length
    self
  end

  def close
    raise IOError, "in 'close'" if @_st_closed
    @_st_closed = true
  end

  def each(&block)
    check_closed
    unless block_given?
      return DirEnumerator.new(self, :each) # for 1.8.7
    end
    i = @_st_index
    lim = @_st_entries.size
    while i < lim
      next_i = i + 1
      @_st_index = next_i
      block.call(@_st_entries[i])
      i = next_i
    end
    self
  end

  def __entries
    @_st_entries
  end

  def path
    # 1.8.7, do not  check_closed
    @_st_path
  end

  def pos
    check_closed
    @_st_index
  end

  def pos=(pos)
    check_closed
    p = Maglev::Type.coerce_to(pos, Fixnum, :to_i)

    @_st_index = p if @_st_range === p
    @_st_index
  end

  def read
    check_closed

    return nil unless @_st_range === @_st_index

    result = @_st_entries[@_st_index]
    @_st_index += 1
    result
  end

  def rewind
    check_closed
    @_st_index = 0
    self 	# change for 1.8.7
  end

  def seek(pos)
    check_closed
    p = Maglev::Type.coerce_to(pos, Fixnum, :to_i)

    @_st_index = p if (0...@_st_entries.size) === p
    self 
  end

  alias tell pos

  def check_closed
    raise IOError, "closed directory" if @_st_closed
  end
end
