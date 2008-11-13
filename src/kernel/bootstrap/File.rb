# -*- coding: utf-8 -*-
# File in Ruby is identically Smalltalk GsFile
class File

    FNM_NOESCAPE = 0x01
    FNM_PATHNAME = 0x02
    FNM_DOTMATCH = 0x04
    FNM_CASEFOLD = 0x08

    SEPARATOR      = '/'
    PATH_SEPARATOR = ':'

    primitive 'close', 'close'
    # << inherited from IO
    primitive 'write', 'addAll:'
    primitive 'next_line', 'nextLineTo:'
    primitive_nobridge '_atEnd', 'atEnd'
    primitive 'read', 'next:'
    primitive 'read', 'contents'

    class_primitive_nobridge '_fstat','fstat:isLstat:'
    class_primitive_nobridge '_stat','stat:isLstat:'
    class_primitive_nobridge '_umask', '_umask:'

    class_primitive_nobridge '_open', 'openOnServer:mode:'
    class_primitive 'stdin'
    class_primitive 'stdout'
    class_primitive 'stderr'
    class_primitive_nobridge '_environmentAt', '_expandEnvVariable:isClient:'
    class_primitive_nobridge '_delete', 'removeServerFile:'

    # For Dir.rb
    class_primitive_nobridge '_dir_contents', 'contentsOfDirectory:onClient:'

    # _modifyFile provides access to chmod, fchmod, chown, lchown, fchown
    class_primitive_nobridge '_modifyFile*', '_modifyFile:fdPath:with:with:'

    def self.name
      'File' # override Smalltalk name
    end

    def self.atime(filename)
      File.stat(filename).atime
    end

    def self.basename(filename, suffix='')
      fn = StringValue(filename)
      sf = StringValue(suffix)
      b = fn.split('/')[-1]
      if suffix.eql?('.*')
        index = b.rindex('.')
      else
        index = b.rindex(suffix)
        if (not index.nil?) && ((index + suffix.size) != b.size)
          index = nil
        end
      end
      return index.nil? ? b : b[0,index]
    end

    def self.blockdev?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.blockdev?
    end

    def self.chardev?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.chardev?
    end

    def self.chmod(permission, *fileNames)
      count = 0
      fileNames.each { |aName|
        status = File._modifyFile( 0, aName, permission, nil)
        if (status.equal?(0))
          count = count + 1
        end
      }
      return count
    end

    def self.chown(owner, group, *fileNames)
      count = 0
      fileNames.each { |aName|
        status = File._modifyFile( 2, aName, owner, group)
        if (status.equal?(0))
          count = count + 1
        end
      }
      return count
    end

    def self.ctime(filename)
      File.stat(filename).ctime
    end

    def self.delete(*filenames)
      filenames.each do |filename|
        path = StringValue(filename)
        # TODO: GsFile(C)>>removeServerFile: returns nil on error: we need errno
        Errno.handle(_delete(filename))
      end
      filenames.size
    end

    def self.directory?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.directory?
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
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.executable?
    end

    def self.executable_real?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.executable_real?
    end

    def self.exist?(path)
      statObj = File._stat(path, false)
      if (statObj._isFixnum)
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
      path = StringValue(a_path) # nil a_path should raise TypeError

      dir = a_dir.nil? ? Dir.pwd : StringValue(a_dir)
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
      return '' if index.nil? || index == (base.size - 1)
      base[index..-1]
    end

    def self.file?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.file?
    end

    # MNI: File.fnmatch
    # MNI: File.fnmatch?

    def self.ftype(filename)
      File.stat(filename).ftype
    end

    def self.grpowned?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.grpowned?
    end

    def self.join(*ary)
        ary.join(SEPARATOR)
    end

    def self.lchmod(permission, *fileNames)
      # not supported , lchmod() not available on Linux or Solaris
      raise NotImplementedError
    end

    def self.lchown(owner, group, *fileNames)
      count = 0
      fileNames.each { |aName|
        status = File._modifyFile( 4, aName, owner, group)
        if (status.equal?(0))
          count = count + 1
        end
      }
      return count
    end

    # MNI: File.link

    def self.lstat(filename)
      _stat(filename, true);
    end

    def self.mtime(filename)
      File.stat(filename).mtime
    end

    def self.new(file, mode="r")
        self._open(file, mode)
    end

    def self.open(file, mode="r", &b)
        f = self._open(file, mode)
        if b
            val = b.call(f)
            f.close
            val
        else
          f
        end
    end

    def self.owned?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.owned?
    end

    def self.pipe?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.pipe?
    end

    def self.readable?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.readable?
    end

    def self.readable_real?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.readable_real?
    end

    # MNI: File.readlink
    # MNI: File.rename

    def self.setgid?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.setgid?
    end

    def self.setuid?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.setuid?
    end

    def self.size(filename)
      File.stat(filename).size
    end

    def self.size?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return nil  # an error attempting to stat
      end
      statObj.size?
    end

    def self.socket?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.socket?
    end

    # MNI: File.split

    def self.stat(filename)
      _stat(filename, false);
    end

    def self.sticky?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.sticky?
    end

    # MNI: File.symlink

    def self.symlink?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.symlink?
    end

    # MNI: File.truncate

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

    # MNI: File.unlink
    # MNI: File.utime

    def self.writable?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.writable?
    end

    def self.writable_real?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.writable_real?
    end

    def self.zero?(filename)
      statObj = File._stat(filename, false)
      if (statObj._isFixnum)
        return false  # an error attempting to stat
      end
      statObj.zero?
    end

    # BEGIN Instance methods

    def atime
      self.stat.atime
    end

    def chmod(permission)
      status = File._modifyFile( 1, @fileDescriptor, permission, nil)
      unless status.equal?(0)
        raise SystemCallError # TODO: Errno::xxx
      end
      return 0
    end

    def chown(owner, group)
      status = File._modifyFile( 3, @fileDescriptor, owner, group)
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

    # MNI: File#flock

    def lchmod(permission)
      # not supported , lchmod() not available on Linux or Solaris
      raise NotImplementedError
    end

    def lchown(owner, group)
      status = File._modifyFile( 4, @pathName, owner, group)
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

    # TODO: The following methods are not documented as part of the API:
    # print, read,
    def print(*args)
        args.each {|arg| self << arg.to_s}
    end

    # TODO: Where is this used?  Not a method
    def self.read(file)
        open(file){|f| f.read}
    end

    def self.read(path)
        file = self.new(path)
        contents = file.read
        file.close
        contents
    end

    def each_line(&block)
        sep = $/[0]
        until eof?
            block.call( next_line( sep ) )
        end
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

    def gets(sep=$/ )
        @block.call.next_line( sep[0] ) #whee
    end

    def sync
        @block.call.sync
    end

    def sync=
        @block.call.sync
    end
end

STDIN = $stdin = PersistentFile.new(proc{File.stdin})
STDOUT = $stdout = PersistentFile.new(proc{File.stdout})
STDERR = $stderr = PersistentFile.new(proc{File.stderr})
$> = $stdout
