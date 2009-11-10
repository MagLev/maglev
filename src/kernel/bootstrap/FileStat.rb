#
# File::Stat in Ruby is identically Smalltalk GsFileStat

class File::Stat

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
    @mode
  end

  def <=>(other)
    return nil unless other.is_a?(File::Stat)
    mtime <=> other.mtime
  end

  def atime
    Time.at @atime
  end

  def blksize
    @blksize
  end

  def blockdev?
    (@mode & S_IFMT) == S_IFBLK
  end

  def blocks
    @blocks
  end

  def chardev?
    (@mode & S_IFMT) == S_IFCHR
  end

  def ctime
    Time.at @ctime
  end

  def dev
    @dev
  end

  def dev_major
    __major(@dev)
  end

  def dev_minor
    __minor(@dev)
  end

  def directory?
    (@mode & S_IFMT) == S_IFDIR
  end

  def executable?
    return true if superuser?
    return @mode & S_IXUSR != 0 if owned?
    return @mode & S_IXGRP != 0 if grpowned?
    return @mode & S_IXOTH != 0
  end

  def executable_real?
    return true if superuser?
    return @mode & S_IXUSR != 0 if rowned?
    return @mode & S_IXGRP != 0 if rgrpowned?
    return @mode & S_IXOTH != 0
  end

  def file?
    (@mode & S_IFMT) == S_IFREG
  end

  def ftype
    case @mode & S_IFMT
    when S_IFBLK : 'blockSpecial'
    when S_IFCHR : 'characterSpecial'
    when S_IFDIR : 'directory'
    when S_IFIFO : 'fifo'
    when S_IFLNK : 'link'
    when S_IFREG : 'file'
    when S_IFSOCK : 'socket'
    else 'unknown'
    end
  end

  def gid
    @gid
  end

  def grpowned?
    @gid == Maglev::System.getegid
  end

  def ino
    @ino
  end

  def inspect
    str = "#<#{self.class.name} dev=0x#{@dev.to_s(16)}, ino=#{@ino},"
    str << " mode=#{sprintf("%07d", @mode.to_s(8).to_i)}, nlink=#{@nlink}, uid=#{@uid},"
    str << " gid=#{@gid}, rdev=0x#{@rdev.to_s(16)}, size=#{@size}, blksize=#{@blksize},"
    str << " blocks=#{@blocks}, atime=#{self.atime}, mtime=#{self.mtime}, ctime=#{self.ctime}>"
    str
  end

  def mtime
    Time.at @mtime
  end

  def nlink
    @nlink
  end

  def owned?
    @uid == Maglev::System.geteuid
  end

  def pipe?
    (@mode & S_IFMT) == S_IFIFO
  end

  def rdev
    @rdev
  end

  def rdev_major
    __major(@rdev)
  end

  def rdev_minor
    __minor(@rdev)
  end

  def readable?
    return true if superuser?
    return @mode & S_IRUSR != 0 if owned?
    return @mode & S_IRGRP != 0 if grpowned?
    return @mode & S_IROTH != 0
  end

  def readable_real?
    return true if superuser?
    return @mode & S_IRUSR != 0 if rowned?
    return @mode & S_IRGRP != 0 if rgrpowned?
    return @mode & S_IROTH != 0
  end

  def setgid?
    (@mode & S_IFMT) == S_ISGID
  end

  def setuid?
    (@mode & S_ISUID) != 0
  end

  def size
    @size
  end

  def size?
    sz = @size
    sz.equal?(0) ? nil : sz
  end

  def socket?
    (@mode & S_IFMT) == S_IFSOCK
  end

  def sticky?
    (@mode & S_ISVTX) != 0
  end

  def symlink?
    (@mode & S_IFMT) == S_IFLNK
  end

  def uid
    @uid
  end

  def writable?
    return true if superuser?
    return @mode & S_IWUSR != 0 if owned?
    return @mode & S_IWGRP != 0 if grpowned?
    return @mode & S_IWOTH != 0
  end

  def writable_real?
    return true if superuser?
    return @mode & S_IWUSR != 0 if rowned?
    return @mode & S_IWGRP != 0 if rgrpowned?
    return @mode & S_IWOTH != 0
  end

  def zero?
    @size.equal?(0)
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
    Maglev::System.getuid == 0
  end

  def rgrpowned?
    @gid == Maglev::System.getgid
  end

  def rowned?
    @uid == Maglev::System.getuid
  end

end
File::Stat.__freeze_constants
