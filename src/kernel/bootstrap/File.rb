# -*- coding: utf-8 -*-
# File in Ruby is identically Smalltalk GsFile
class File

  ALT_SEPARATOR  = nil
  PATH_SEPARATOR = ':'
  SEPARATOR      = '/'
  Separator      = SEPARATOR

  Stat = _resolve_smalltalk_global(:GsFileStat)

  class_primitive_nobridge '_section2OpenConstants', '_section2OpenConstants'

  # Note, these constants are OS independent values different than
  #  any found on common variants of Unix. They are translated to 
  #  OS dependent values by the primitives.  Use of other hardcoded values
  #  will cause Exceptions during file openinng.
  APPEND = self._section2OpenConstants[:RUBY_APPEND]
  CREAT = self._section2OpenConstants[:RUBY_CREAT]
  EXCL = self._section2OpenConstants[:RUBY_EXCL]
  NOCTTY = self._section2OpenConstants[:RUBY_NOCTTY]
  NONBLOCK = self._section2OpenConstants[:RUBY_NONBLOCK]
  RDONLY = self._section2OpenConstants[:RUBY_RDONLY]
  RDWR = self._section2OpenConstants[:RUBY_RDWR]
  TRUNC = self._section2OpenConstants[:RUBY_TRUNC]
  WRONLY = self._section2OpenConstants[:RUBY_WRONLY]
  # 

  # FILE::LOCK  constants initialized below

  primitive 'close', 'close'
  # << inherited from IO
  primitive 'write', 'addAll:'
  primitive 'next_line', 'nextLineTo:'
  primitive_nobridge '_atEnd', 'atEnd'
  primitive 'read', 'next:'
  primitive 'read', 'contents'
  primitive 'flush', 'flush'
  primitive 'rewind', 'rewind'
  primitive_nobridge '_seek', '_seekTo:opcode:'

  class_primitive_nobridge '_fstat','fstat:isLstat:'

  # _stat will return either a stat object or ERRNO
  class_primitive_nobridge '__stat','stat:isLstat:'
  class_primitive_nobridge '_umask', '_umask:'

  class_primitive_nobridge '_open', '_rubyOpen:mode:permission:'  # 1st is String, 2nd,3rd are ints
  class_primitive_nobridge '_fopen', '_rubyFopen:mode:' # path is String or Fixnum, mode is a String
				      # first arg determines use of fdopen or open
  class_primitive 'stdin'
  class_primitive 'stdout'
  class_primitive 'stderr'
  class_primitive_nobridge '_environmentAt', '_expandEnvVariable:isClient:'
  class_primitive_nobridge '_delete', 'removeServerFile:'


  # For Dir.rb
  class_primitive_nobridge '_dir_contents', 'contentsOfDirectory:onClient:'

  # _modify_file provides access to chmod, fchmod, chown, lchown, fchown
  class_primitive '_modify_file*', '_modifyFile:fdPath:with:with:'

  def self.atime(filename)
    File.stat(filename).atime
  end

  def each(separator=$/, &block)
    sep = separator[0]
    until eof?
      block.call(next_line(sep))
    end
  end

  def self._stat(name, is_lstat)
    unless name.equal?(nil)
      name = Type.coerce_to(name, String, :to_str)
    end
    __stat(name, is_lstat)
  end

  # TODO: consider using FFI to call libc basename
  def self.basename(filename, suffix='')
    fn = Type.coerce_to(filename, String, :to_str).squeeze('/')
    sf = Type.coerce_to(suffix, String, :to_str)

    return '/' if fn.eql?('/')

    b = fn.split('/')[-1]
    return '' if b.equal?(nil)

    if suffix.eql?('.*')
      index = b.rindex('.')
    else
      index = b.rindex(suffix)
      if (not index.equal?(nil)) && ((index + suffix.size) != b.size)
        index = nil
      end
    end
    return index.equal?(nil) ? b : b[0,index]
  end

  def self.blockdev?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.blockdev?
  end

  def self.chardev?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.chardev?
  end

  def self.chmod(permission, *file_names)
    count = 0
    file_names.each { |a_name|
      status = File._modify_file( 1, a_name, permission, nil)
      if (status.equal?(0))
        count = count + 1
      end
    }
    return count
  end

  def self.chown(owner, group, *file_names)
    count = 0
    file_names.each { |a_name|
      status = File._modify_file( 3, a_name, owner, group)
      if (status.equal?(0))
        count = count + 1
      end
    }
    return count
  end

  def self.ctime(filename)
    File.stat(filename).ctime
  end

  def self.delete(*file_names)
    count = 0
    file_names.each { |a_name|
      status = File._modify_file( 0, a_name, nil, nil )
      if (status.equal?(0))
        count = count + 1
      end
    }
    return count
  end

  def self.directory?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.directory?
  end

  def self.dirname(str)
    if str =~ /(.+)\//
      $1
    else
      if str[0] == ?/
        "/"
      else
        "."
      end
    end
  end

  def self.executable?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.executable?
  end

  def self.executable_real?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.executable_real?
  end

  def self.exist?(path)
    stat_obj = File._stat(path, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    true
  end

  class << self
    alias exists? exist?
  end

  # Convert path to an absolute pathname. If no directory is given,
  # relative paths are rooted at the current working directory.
  # Otherwise directory will be prefixed to the path. Tilde expansion is
  # done on the path.  Replaces '.' and '..' in the path with the
  # appropriate path components.  Does not coalesce contiguous '/'s.
  def self.expand_path(a_path, a_dir = nil)
    path = Type.coerce_to(a_path, String, :to_str) # nil a_path should raise TypeError

    dir = a_dir.equal?(nil) ? Dir.pwd : Type.coerce_to(a_dir, String, :to_str)
    dir = Dir.pwd if dir.empty?

    return dir if path.empty?

    path = _tilde_expand(path)

    if path[0] !=  SEPARATOR[0] # relative path
      path = File.join(dir, path)
    end

    _cannonicalize(path)
  end

  # Remove .. and . from path
  def self._cannonicalize(path)
    return path unless path['.']
    new_parts = []
    path.split(SEPARATOR).each do |component|
      case component
      when '..'
        new_parts.pop
      when '.'
        # nothing: just skip it
      when ''
        # The split
      else
        new_parts << component
      end
    end
    "#{SEPARATOR}#{new_parts.join(SEPARATOR)}"
  end

  def self._tilde_expand(path)
    case path[0,2]
    when '~':   ENV['HOME']
    when '~/':  ENV['HOME'] + path[1..-1]
    when /^~([^\/])+(.*)/
      raise NotImplementedError, "Don't handle ~user expansion yet"
    else
      path
    end
  end

  def self.extname(filename)
    base = self.basename(filename)
    index = base.rindex('.')
    return '' if index.equal?(nil) || index == (base.size - 1)
    base[index..-1]
  end

  def self.file?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.file?
  end

  # MNI: File.fnmatch
  # MNI: File.fnmatch?

  def self.ftype(*names)
    unless names.length.equal?(1)
      raise ArgumentError , 'expected 1 arg'
    end
    File.stat(names[0]).ftype
  end

  def self.grpowned?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.grpowned?
  end

  def self.identical?(file_1, file_2)
    stat_1 = File._stat(Type.coerce_to(file_1, String, :to_str))
    stat_2 = File._stat(Type.coerce_to(file_2, String, :to_str))
    return false unless stat_1.ino == stat_2.ino
    return false unless stat_1.ftype == stat_2.ftype
#     return false unless POSIX.access(orig, Constants::R_OK)
#     return false unless POSIX.access(copy, Constants::R_OK)
    return true
  end

  def self.lchmod(permission, *file_names)
    # not supported , lchmod() not available on Linux or Solaris
    raise NotImplementedError
  end

  def self.lchown(owner, group, *file_names)
    count = 0
    file_names.each { |a_name|
      status = File._modify_file( 4, a_name, owner, group)
      if (status.equal?(0))
        count = count + 1
      end
    }
    return count
  end

  def self.link(oldname, newname)
    status = File._modify_file(8, oldname, newname)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
  end

  def self.lstat(filename)
    _stat(filename, true);
  end

  def self.mtime(filename)
    File.stat(filename).mtime
  end

  def self.new(filename, mode, permission)
    unless mode._isFixnum
      raise TypeError, 'second arg not a Fixnum '
    end
    unless permission._isFixnum
      raise TypeError, 'third arg not a Fixnum '
    end
    f = self._open(filename, mode, permission)
    if f._isFixnum
      Errno.raise_errno(f, filename )
    end
    f.initialize
    f
  end

  def self.new(filename, mode)
    if mode._isString
      f = self._fopen(filename, mode)
    elsif mode._isFixnum
      f = self._open(filename, mode, -1)  # fdopen() or open()
    else
      raise TypeError, 'second arg neither Fixnum nor String'
    end
    if f._isFixnum
      Errno.raise_errno(f, filename )
    end
    f.initialize
    f
  end

  def self.new(filename)
    filename = Type.coerce_to(filename, String, :to_str)
    self.new(filename, 'r')
  end

  def self.open(filename, mode, permission, &blk)
    unless mode._isFixnum
      raise TypeError, 'second arg not a Fixnum '
    end
    unless permission._isFixnum
      raise TypeError, 'third arg not a Fixnum '
    end
    f = self._open(filename, mode, permission)
    if f._isFixnum
      Errno.raise_errno(f, filename )
    end
    if block_given?
      begin
        blk.call(f)
      ensure
        f.close rescue nil
      end
    else
      f 
    end
  end

  def self.open(filename, mode, &blk)
    if mode._isString
      f = self._fopen(filename, mode)
    elsif mode._isFixnum
      f = self._open(filename, mode, -1)
    else
      raise TypeError, 'second arg neither Fixnum nor String'
    end
    if f._isFixnum
      Errno.raise_errno(f, filename )
    end
    if block_given?
      begin
        blk.call(f)
      ensure
        f.close rescue nil
      end
    else
      f
    end
  end 

  def self.open(filename, mode)
    if mode._isString
      f = self._fopen(filename, mode)
    elsif mode._isFixnum
      f = self._open(filename, mode, -1)
    else
      raise TypeError, 'second arg neither Fixnum nor String'
    end
    if f._isFixnum
      Errno.raise_errno(f, filename )
    end
    f
  end 

  def self.open(filename, &blk)
    filename = Type.coerce_to(filename, String, :to_str)
    self.open(filename, 'r', &blk)
  end

  def self.open(filename)
    filename = Type.coerce_to(filename, String, :to_str)
    self.open(filename, 'r')
  end

  def self.owned?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.owned?
  end

  def self.pipe?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.pipe?
  end

  def self.readable?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.readable?
  end

  def self.readable_real?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.readable_real?
  end

  def self.readlink(filename)
    res = String.new
    status = File._modify_file(9, filename, res)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
    res
  end

  def self.rename(oldname, newname)
    status = File._modify_file(7, oldname, newname)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
  end

  def self.setgid?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.setgid?
  end

  def self.setuid?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.setuid?
  end

  def self.size(filename)
    File.stat(filename).size
  end

  def self.size?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return nil  # an error attempting to stat
    end
    stat_obj.size?
  end

  def self.socket?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.socket?
  end

  # MNI: File.split

  def self.stat(filename)
    stat_obj = Errno.handle(_stat(filename, false), filename)
#     stat_obj = _stat(filename, false);
#     if (stat_obj._isFixnum)
#       raise Errno::ENOENT
# #      raise SystemCallError # TODO: Errno::xxx
#     end
    stat_obj
  end

  def self.sticky?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.sticky?
  end

  def self.symlink(oldname, newname)
    status = File._modify_file(6, oldname, newname)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
  end

  def self.symlink?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.symlink?
  end

  def self.truncate(filename, newsize)
    status = File._modify_file(2, filename, newsize)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
  end

  def self.umask
    # return current file creation mask
    _umask(-1)
  end

  def self.umask(newMask)
    # set file creation mask to newMask and return previous value
    # newMask must be >= 0 and <= 0777
    if (newMask >= 0)
      res = _umask(newMask)
    else
      res = -1
    end
    if (res < 0)
      raise RangeError
    end
    res
  end

  def self.unlink(*file_names)
    delete(*file_names)
  end

  def self.utime(accesstime, modtime, *filenames)
    count = 0
    filenames.each { |a_name|
      status = File._modify_file( 5, a_name, accesstime, modtime)
      if (status.equal?(0))
        count = count + 1
      end
    }
    return count
  end

  def self.writable?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.writable?
  end

  def self.writable_real?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.writable_real?
  end

  def self.zero?(filename)
    stat_obj = File._stat(filename, false)
    if (stat_obj._isFixnum)
      return false  # an error attempting to stat
    end
    stat_obj.zero?
  end

  # BEGIN Instance methods

  def atime
    self.stat.atime
  end

  def chmod(permission)
    status = File._modify_file( 10, @fileDescriptor, permission, nil)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
    return 0
  end

  def chown(owner, group)
    status = File._modify_file( 12, @fileDescriptor, owner, group)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
    return 0
  end

  def ctime
    self.stat.ctime
  end

  def eof?
    status = self._atEnd
    if (status.equal?(nil))
      raise IOError
    end
    status
  end

  # begin gets implementation --------------------------------------------

  def gets(*args)
    raise ArgumentError, 'expected 0 or 1 arg'
  end

  def gets(sep)
    # variant after first gets no bridges
    res = next_line( sep[0] )
    res._storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def gets
    # variant after first gets no bridges
    sep=$/
    res = next_line( sep[0] )
    res._storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  # during bootstrap,  send and __send__ get no bridge methods
  def send(sym)
    if (sym.equal?(:gets))
      sep=$/
      res = next_line( sep[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym)
  end

  def send(sym, arg)
    if (sym.equal?(:gets))
      res = next_line( arg[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym, arg)
  end

  def __send__(sym)
    if (sym.equal?(:gets))
      sep=$/
      res = next_line( sep[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym)
  end

  def __send__(sym, arg)
    if (sym.equal?(:gets))
      res = next_line( arg[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym, arg)
  end

  # end gets --------------------------------------------------


  def self.fetch_flock_constants
    # returns [ LOCK_EX, LOCK_NB, LOCK_SH, LOCK_UN ] from VM
    #  they will be -1 on Solaris , where flock not supported
    arr = [ ]
    status = File._modify_file(13, 0, arr)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
    arr
  end

  LOCK_EX = fetch_flock_constants()[0]
  LOCK_NB = fetch_flock_constants()[1]
  LOCK_SH = fetch_flock_constants()[2]
  LOCK_UN = fetch_flock_constants()[3]

  def flock(lock_constant)
    status = File._modify_file(11, @fileDescriptor, lock_constant)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
  end


  def lchmod(permission)
    # not supported , lchmod() not available on Linux or Solaris
    raise NotImplementedError
  end

  def lchown(owner, group)
    status = File._modify_file( 4, @pathName, owner, group)
    unless status.equal?(0)
      raise SystemCallError # TODO: Errno::xxx
    end
    return 0
  end

  def lstat
    File._stat(@pathName, true)
  end

  def mtime
    self.stat.mtime
  end

  def path
    @pathName
  end

  def stat
    res = File._fstat(@fileDescriptor, false)
    if (res._isFixnum)
      raise SystemCallError # TODO: Errno::xxx
    end
    return res
  end

  def each_line(&block)
    sep = ($/.equal?(nil) ? 10 : $/[0])
    until eof?
      block.call( next_line( sep ) )
    end
  end

  def seek(offset, whence = IO::SEEK_SET)
    _seek(offset, whence)
  end

  # Return the current offset (in bytes) of +io+
  primitive_nobridge 'pos', 'position'

  # Seeks to the given position (in bytes) in +io+
  def pos=(offset)
      seek(offset, IO::SEEK_SET)
  end

end

class PersistentFile
  def initialize(block)
    @block = block
  end

  def _file
    @block.call
  end

  def <<(data)
    _file << data
  end

  def print(*args)
    _file.print(*args)
  end

  def printf(format, *args)
    _file.printf(format, *args)
  end

  def putc(arg)
    _file.putc(args)
  end

  def puts(*args)
    _file.puts(*args)
  end

  def puts(*args)
    _file.puts(*args)
  end

  def write(data)
    _file.write(data)
  end

  def flush
    _file.flush
  end

  # begin gets implementation -------------------------------
  def gets(*args)
    raise ArgumentError, 'expected 0 or 1 args'
  end

  def gets
    # variants after first get no bridge methods
    sep = $/
    res = if sep.equal?(nil)
            # Read entire file
            raise NotImplementedError, 'Kernel#gets does not support full file mode'
          elsif sep.length.equal?(0)
            # Read by paragraphs
            raise NotImplementedError, 'Kernel#gets does not support paragraph mode'
          else
            # read by lines
            @block.call.next_line( sep[0] )
          end
    res._storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def gets(sep )
    # variants after first get no bridge methods
    res = @block.call.next_line( sep[0] )
    res._storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  # during bootstrap,  send and __send__ get no bridge methods
  def send(sym)
    if (sym.equal?(:gets))
      sep = $/
      res = @block.call.next_line( sep[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym)
  end

  def send(sym, arg)
    if (sym.equal?(:gets))
      res = @block.call.next_line( arg[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym, arg)
  end

  def __send__(sym)
    if (sym.equal?(:gets))
      sep = $/
      res = @block.call.next_line( sep[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym)
  end

  def __send__(sym, arg)
    if (sym.equal?(:gets))
      res = @block.call.next_line( arg[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym, arg)
  end

  # end gets implementation -------------------------------

  def sync
    @block.call.sync
  end

  def sync=(bool)
    @block.call.sync=(bool)
  end

  # Returns true if io is associated with a terminal device (tty), and
  # returns false otherwise.
  def isatty
    false
  end
  alias tty? isatty
end

# STDIN, STDOUT, STDERR , $>  initialized in File2.rb
