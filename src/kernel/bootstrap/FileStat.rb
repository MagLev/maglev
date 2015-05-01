#
# File::Stat in Ruby is identically Smalltalk GsFileStat

class File
 class Stat

  include Comparable

  # POSIX constants for accessing the mode field
  S_IFMT   = 0170000  #  type of file mask
  S_IFIFO  = 0010000  #  named pipe (fifo)
  S_IFCHR  = 0020000  #  character special
  S_IFDIR  = 0040000  #  directory
  S_IFBLK  = 0060000  #  block special
  S_IFREG  = 0100000  #  regular
  S_IFLNK  = 0120000  #  symbolic link
  S_IFSOCK = 0140000  #  socket

  S_ISUID  = 0004000  # set user id on execution
  S_ISGID  = 0002000  # set group id on execution
  S_ISVTX  = 0001000  # directory restrcted delete

  S_IRWXU  = 0000700  # RWX mask for owner
  S_IRUSR  = 0000400  # R for owner
  S_IWUSR  = 0000200  # W for owner
  S_IXUSR  = 0000100  # X for owner

  S_IRWXG  = 0000070  # RWX mask for group
  S_IRGRP  = 0000040  # R for group
  S_IWGRP  = 0000020  # W for group
  S_IXGRP  = 0000010  # X for group

  S_IRWXO  = 0000007  # RWX mask for other
  S_IROTH  = 0000004  # R for other
  S_IWOTH  = 0000002  # W for other
  S_IXOTH  = 0000001  # X for other

  # def self.name ; end # not needed, see code in Module>>initNameSpacesForExtend:

  def self.new(filename)
    File.stat(filename)
  end

  def mode
    @_st_mode
  end

  def <=>(other)
    return nil unless other._is_a?(Stat)  # Stat resolves to File::Stat
    mtime <=> other.mtime
  end

  def atime
    Time.at @_st_atime
  end

  def blksize
    @_st_blksize
  end

  def blockdev?
    (@_st_mode & S_IFMT) == S_IFBLK
  end

  def blocks
    @_st_blocks
  end

  def chardev?
    (@_st_mode & S_IFMT) == S_IFCHR
  end

  def ctime
    Time.at @_st_ctime
  end

  def dev
    @_st_dev
  end

  def dev_major
    __major(@_st_dev)
  end

  def dev_minor
    __minor(@_st_dev)
  end

  def directory?
    (@_st_mode & S_IFMT) == S_IFDIR
  end

  def executable?
    return true if superuser?
    return @_st_mode & S_IXUSR != 0 if owned?
    return @_st_mode & S_IXGRP != 0 if grpowned?
    return @_st_mode & S_IXOTH != 0
  end

  def executable_real?
    return true if superuser?
    return @_st_mode & S_IXUSR != 0 if rowned?
    return @_st_mode & S_IXGRP != 0 if rgrpowned?
    return @_st_mode & S_IXOTH != 0
  end

  def file?
    (@_st_mode & S_IFMT) == S_IFREG
  end

  def ftype
    case @_st_mode & S_IFMT
    when S_IFBLK  then 'blockSpecial'
    when S_IFCHR  then 'characterSpecial'
    when S_IFDIR  then 'directory'
    when S_IFIFO  then 'fifo'
    when S_IFLNK  then 'link'
    when S_IFREG  then 'file'
    when S_IFSOCK then 'socket'
    else 'unknown'
    end
  end

  def gid
    @_st_gid
  end

  def grpowned?
    @_st_gid == Maglev.__system.getegid
  end

  def ino
    @_st_ino
  end

  def inspect
    str = "#<#{self.class.name} dev=0x#{@_st_dev.to_s(16)}, ino=#{@_st_ino},"
    str << " mode=#{sprintf("%07d", @_st_mode.to_s(8).to_i)}, nlink=#{@_st_nlink}, uid=#{@_st_uid},"
    str << " gid=#{@_st_gid}, rdev=0x#{@_st_rdev.to_s(16)}, size=#{@_st_size}, blksize=#{@_st_blksize},"
    str << " blocks=#{@_st_blocks}, atime=#{self.atime}, mtime=#{self.mtime}, ctime=#{self.ctime}>"
    str
  end

  def mtime
    Time.at @_st_mtime
  end

  def nlink
    @_st_nlink
  end

  def owned?
    @_st_uid == Maglev.__system.geteuid
  end

  def pipe?
    (@_st_mode & S_IFMT) == S_IFIFO
  end

  def rdev
    @_st_rdev
  end

  def rdev_major
    __major(@_st_rdev)
  end

  def rdev_minor
    __minor(@_st_rdev)
  end

  def readable?
    return true if superuser?
    return @_st_mode & S_IRUSR != 0 if owned?
    return @_st_mode & S_IRGRP != 0 if grpowned?
    return @_st_mode & S_IROTH != 0
  end

  def readable_real?
    return true if superuser?
    return @_st_mode & S_IRUSR != 0 if rowned?
    return @_st_mode & S_IRGRP != 0 if rgrpowned?
    return @_st_mode & S_IROTH != 0
  end

  def setgid?
    (@_st_mode & S_IFMT) == S_ISGID
  end

  def setuid?
    (@_st_mode & S_ISUID) != 0
  end

  def size
    @_st_size
  end

  def size?
    sz = @_st_size
    sz._equal?(0) ? nil : sz
  end

  def socket?
    (@_st_mode & S_IFMT) == S_IFSOCK
  end

  def sticky?
    (@_st_mode & S_ISVTX) != 0
  end

  def symlink?
    (@_st_mode & S_IFMT) == S_IFLNK
  end

  def uid
    @_st_uid
  end

  def writable?
    return true if superuser?
    return @_st_mode & S_IWUSR != 0 if owned?
    return @_st_mode & S_IWGRP != 0 if grpowned?
    return @_st_mode & S_IWOTH != 0
  end

  def writable_real?
    return true if superuser?
    return @_st_mode & S_IWUSR != 0 if rowned?
    return @_st_mode & S_IWGRP != 0 if rgrpowned?
    return @_st_mode & S_IWOTH != 0
  end

  def zero?
    @_st_size._equal?(0)
  end

  # pull the major device number out of a dev_t
  def __major(dev_t)
    (dev_t >> 24) & 0x0ff
  end
  # pull the minor device number out of a dev_t
  def __minor(dev_t)
    dev_t & 0xffffff
  end

  def superuser?
    Maglev.__system.getuid == 0
  end

  def rgrpowned?
    @_st_gid == Maglev.__system.getgid
  end

  def rowned?
    @_st_uid == Maglev.__system.getuid
  end

 end
end
File::Stat.__freeze_constants
