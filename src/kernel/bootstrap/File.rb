# File in Ruby is identically Smalltalk GsFile
class File
    primitive 'close', 'close'
    primitive '<<', 'addAll:'
    primitive 'write', 'addAll:'
    primitive 'next_line', 'nextLineTo:'
    primitive 'eof?', 'atEnd'
    primitive 'read', 'next:'
    primitive 'read', 'contents'

    class_primitive_nobridge '_fstat','fstat:isLstat:'
    class_primitive_nobridge '_stat','stat:isLstat:'

    class_primitive_nobridge '_open', 'openOnServer:mode:'
    class_primitive 'stdin'
    class_primitive 'stdout'
    class_primitive 'stderr'
    class_primitive_nobridge '_environmentAt', '_expandEnvVariable:isClient:'

    # _modifyFile provides access to chmod, fchmod, chown, lchown, fchown
    class_primitive_nobridge '_modifyFile*', '_modifyFile:fdPath:with:with:'

    def self.name
      # override Smalltalk name
      'File'
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

    def self.atime(filename)
      File.stat(filename).atime
    end

    def self.blockdev?(filename)
      File.stat(filename).blockdev?
    end

    def self.chardev?(filename)
      File.stat(filename).chardev?
    end

    def self.ctime(filename)
      File.stat(filename).ctime
    end

    def self.directory?(filename)
      File.stat(filename).directory?
    end

    def self.file?(filename)
      File.stat(filename).file?
    end

    def self.mtime(filename)
      File.stat(filename).mtime
    end

    def self.pipe?(filename)
      File.stat(filename).pipe?
    end

    def self.size(filename)
      File.stat(filename).size
    end

    def self.size?(filename)
      File.stat(filename).size?
    end

    def self.socket?(filename)
      File.stat(filename).socket?
    end

    def self.sticky?(filename)
      File.stat(filename).sticky?
    end

    def self.symlink?(filename)
      File.stat(filename).symlink?
    end

    def self.zero?(filename)
      File.stat(filename).zero?
    end


    def print(*args)
        args.each {|arg| self << arg.to_s}
    end

    def self.read(file)
        open(file){|f| f.read}
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

    def self.join(*ary)
        ary.join("/")
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

    def self.extname(filename)
      base = self.basename(filename)
      index = base.rindex('.')
      return '' if index.nil? || index == (base.size - 1)
      base[index..-1]
    end

    def self.stat(filename)
      _stat(filename, false);
    end

    def self.lstat(filename)
      _stat(filename, true);
    end

    def stat
      File._fstat(@fileDescriptor, false)
    end

    def lstat
      File._stat(@pathName, true)
    end

    def chmod(permission)
      status = File._modifyFile( 1, @fileDescriptor, permission, nil)
      unless status.equal?(0)
        raise SystemCallError # TODO: Errno::xxx 
      end
      return 0 
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

    def chown(owner, group)
      status = File._modifyFile( 3, @fileDescriptor, owner, group)
      unless status.equal?(0)
        raise SystemCallError # TODO: Errno::xxx 
      end
      return 0 
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
  
    def lchmod(permission)
      # not supported , lchmod() not available on Linux or Solaris
      raise NotImplementedError
    end

    def self.lchmod(permission, *fileNames)
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

      
end

class PersistentFile
    def initialize(block)
        @block = block
    end

    def _file
        @block.call
    end

    def print(*args)
        args.each {|arg| self << arg.to_s}
    end

    def <<(data)
        _file << data
    end

    def write(data)
      _file << data
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
