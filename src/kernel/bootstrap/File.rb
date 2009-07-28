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
  primitive '_write', 'addAll:'
  primitive '_getc', 'nextByte'
  primitive '_next_line_to', 'nextLineTo:'
  primitive_nobridge '_atEnd', 'atEnd'
  primitive '_read', 'next:'
  primitive '_read', 'contents'
  primitive '_read_into', 'read:into:'
  primitive 'flush', 'flush'
  primitive 'rewind', 'rewind'
  primitive '_is_open', 'isOpen'
  primitive '_last_err_string', 'lastErrorString'
  primitive '_last_err_code', 'lastErrorCode'
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
      block.call(_next_line(sep))
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


  def self.new(filename, mode=Undefined, permission=Undefined)
    self.open(filename, mode, permission)
  end

  def self.open(filename, mode=Undefined, permission=Undefined, &blk)
    if filename._isInteger
      raise TypeError , 'File.new(fd_integer)  not supported yet'
    end
    filename = Type.coerce_to(filename, String, :to_str)
    nargs = 1
    if mode.equal?(Undefined)
      mode = 'r'
      nargs = 2
    else
      if permission.equal?(Undefined)
        nargs = 2
      else
        unless permission._isFixnum
          raise TypeError, 'third arg not a Fixnum '
        end
        nargs = 3
      end
    end
    if nargs.equal?(2)
      if mode._isString
        f = self._fopen(filename, mode)
      elsif mode._isFixnum
        f = self._open(filename, mode, -1)
      else
        raise TypeError, 'second arg neither Fixnum nor String'
      end
    else
      if mode._isFixnum
        f = self._open(filename, mode, permission)
      else
        stat_obj = File._stat(filename, false) # does file exist before we create?
        f = self._fopen(filename, mode)
        Errno.raise_errno(stat_obj, filename) if f.equal?(nil)
        f.chmod(permission) if stat_obj._isFixnum # chmod new files (if couldn't stat it)
      end
    end

    Errno.raise_errno(f, filename ) if f._isFixnum

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

  def closed?
    not _is_open
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

  def getc
    raise IOError, 'getc: closed stream' unless _is_open
    return nil if self.eof?
    _getc
  end

  def gets(*args)
    raise ArgumentError, 'expected 0 or 1 arg'
  end

  def gets(sep)
    # variant after first gets no bridges
    res = _next_line( sep[0] )
    res._storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def gets
    # variant after first gets no bridges
    sep=$/
    res = _next_line( sep[0] )
    res._storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def _next_line(sep)
    res = _next_line_to(sep)
    if res.equal?(nil)
      unless _last_err_code.equal?(0)
        raise IOError , self._last_err_string  # TODO: Errno::xxx
      end
    end
    res
  end

  def read(a_length=Undefined, a_buffer=Undefined)
    raise IOError, 'read: closed stream' unless _is_open

    read_all_bytes = a_length.equal?(Undefined) || a_length.nil?
    unless read_all_bytes
      length = Type.coerce_to(a_length, Fixnum, :to_int)
      raise ArgumentError, "length must not be negative" if length < 0
      return nil if self.pos > self.stat.size
    end

    if self.eof?
      return read_all_bytes ? '' : nil
    end

    data = if a_buffer.equal?(Undefined)
             data = read_all_bytes ? _read : _read(length)
           else
             buffer = Type.coerce_to(a_buffer, String, :to_str)
             length = self.stat.size if length.equal?(nil)
             num_read = _read_into(length, buffer)
             raise IOError, 'error' if num_read.equal?(nil)
             buffer.size = num_read # truncate buffer
             buffer
           end
    data = '' if data.equal?(nil)
    data
  end

  # during bootstrap,  send and __send__ get no bridge methods
  def send(sym)
    if (sym.equal?(:gets))
      sep=$/
      res = _next_line( sep[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym)
  end

  def send(sym, arg)
    if (sym.equal?(:gets))
      res = _next_line( arg[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym, arg)
  end

  def __send__(sym)
    if (sym.equal?(:gets))
      sep=$/
      res = _next_line( sep[0] )
      res._storeRubyVcGlobal(0x21) # store into caller's $_
      return res
    end
    super(sym)
  end

  def __send__(sym, arg)
    if (sym.equal?(:gets))
      res = _next_line( arg[0] )
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
      block.call( _next_line( sep ) )
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

  def write(arg)
    count = self._write(arg)
    if count.equal?(nil)
      raise IOError , self._last_err_string  # TODO: Errno::xxx
    end
    count
  end

end
File._freeze_constants

# STDIN, STDOUT, STDERR , $>  initialized in File2.rb
