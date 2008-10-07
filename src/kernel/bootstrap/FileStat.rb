#
# File::Stat in Ruby is identically Smalltalk GsFileStat

class File::Stat

  include Comparable

  def self.name
    'File::Stat'   # override Smalltalk name
  end

  # POSIX constants for accessing the mode field
  S_IFMT   = 0170000  #  type of file mask
  S_IFIFO  = 0010000  #  named pipe (fifo)
  S_IFCHR  = 0020000  #  character special
  S_IFDIR  = 0040000  #  directory
  S_IFBLK  = 0060000  #  block special
  S_IFREG  = 0100000  #  regular
  S_IFLNK  = 0120000  #  symbolic link
  S_IFSOCK = 0140000  #  socket

  S_ISUID = 0004000   # set user id on execution
  S_ISGID = 0002000   # set group id on execution
  S_ISVTX = 0001000   # directory restrcted delete

  primitive '_atime', 'atimeUtcSeconds'
  primitive '_ctime', 'ctimeUtcSeconds'
  primitive '_mtime', 'mtimeUtcSeconds'
  primitive 'mode', 'mode'

  # MNI: FileStat: <=>

  def <=>(other)
    return nil unless other.is_a?(File::Stat)
    self.mtime <=> other.mtime
  end

  def atime
    Time.at _atime
  end

  primitive 'blksize', 'blksize'

  def blockdev?
    (mode & S_IFMT) == S_IFBLK
  end

  primitive 'blocks', 'blocks'

  def chardev?
    (mode & S_IFMT) == S_IFCHR
  end

  def ctime
    Time.at _ctime
  end

  primitive 'dev', 'dev'

  def dev_major
    _major(dev)
  end

  def dev_minor
    _minor(dev)
  end

  def directory?
    (mode & S_IFMT) == S_IFDIR
  end

  # MNI: FileStat: executable?
  # MNI: FileStat: executable_real?

  def file?
    (mode & S_IFMT) == S_IFREG
  end

  def ftype
    case mode & S_IFMT
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

  primitive 'gid', 'gid'

  # MNI: FileStat: grpowned?

  primitive 'ino', 'ino'

  def mtime
    Time.at _mtime
  end

  primitive 'nlink', 'nlink'

  # MNI: FileStat: owned?

  def pipe?
    (mode & S_IFMT) == S_IFIFO
  end

  primitive 'rdev', 'rdev'

  def rdev_major
    _major(rdev)
  end

  def rdev_minor
    _minor(rdev)
  end

  # MNI: FileStat: readable?
  # MNI: FileStat: readable_real?

  def setgid?
    (mode & S_IFMT) == S_ISGID
  end

  def setuid?
    (mode & S_ISUID) != 0
  end

  primitive 'size', 'size'

  def size?
    size == 0 ? nil : size
  end

  def socket?
    (mode & S_IFMT) == S_IFSOCK
  end

  def sticky?
    (mode & S_ISVTX) != 0
  end

  def symlink?
    (mode & S_IFMT) == S_IFLNK
  end

  primitive 'uid', 'uid'

  # MNI: FileStat: writable?
  # MNI: FileStat: writable_real?

  def zero?
    size == 0
  end

  # pull the major device number out of a dev_t
  def _major(dev_t)
    (dev_t >> 24) & 0x0ff
  end
  # pull the minor device number out of a dev_t
  def _minor(dev_t)
    dev_t & 0xffffff
  end

end
