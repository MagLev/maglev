# File in Ruby is identically Smalltalk GsFile
class File
    primitive 'close', 'close'
    primitive '<<', 'addAll:'
    primitive 'write', 'addAll:'
    primitive 'next_line', 'nextLineTo:'
    primitive 'eof?', 'atEnd'
    primitive 'read', 'next:'
    primitive 'read', 'contents'
    self.class.primitive_nobridge '_open', 'openOnServer:mode:'
    self.class.primitive 'stdin'
    self.class.primitive 'stdout'
    self.class.primitive 'stderr'
    self.class.primitive_nobridge '_environmentAt', '_expandEnvVariable:isClient:'

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

    self.class.primitive '_fileKind', '_fileKind:onClient:'
    def self.blockdev?(filename)
      _fileKind(filename, true) == 3
    end
    def self.chardev?(filename)
      _fileKind(filename, true) == 2
    end
    def self.directory?(filename)
      # TODO: this doesn't work if the directory is a symlink
      _fileKind(filename, true) == 1
    end
    def self.file?(filename)
      _fileKind(filename, true) == 0
    end
    def self.symlink?(filename)
      # TODO: this doesn't work if the symlink points to a directory
      _fileKind(filename, true) == 4
    end

    def self.basename(filename, suffix='')
      fn = StringValue(filename)
      sf = StringValue(suffix)
      b = fn.split('/')[-1]
      # This also works if suffix is ''
      index = filename.rindex(suffix.eql?('.*') ? '.' : suffix)
      return index.nil? ? filename : filename[0,index]
    end

    def self.extname(filename)
      base = self.basename(filename)
      (result = base[/\..*$/]).nil? ? '' : result
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
