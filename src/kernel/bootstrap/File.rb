# File in Ruby is identically Smalltalk GsFile  
class File
    primitive 'close', 'close'
    primitive '<<', 'addAll:'
    primitive 'write', 'addAll:'
    primitive 'next_line', 'nextLineTo:'
    primitive 'eof?', 'atEnd'
    primitive 'read', 'next:'
    primitive 'read', 'contents'
    self.class.primitive '_open', 'openOnServer:mode:'
    self.class.primitive 'stdin'
    self.class.primitive 'stdout'
    self.class.primitive 'stderr'

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
        
    # workaround for Ticket 67 where $/  not working yet, use 10
    def each_line(&block)
        until eof?
            block.call( next_line( 10 ) )
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
    
    def print(*args)
        args.each {|arg| self << arg.to_s}
    end
    
    def <<(data)
        _file << data
    end
    
    def write(data)
      _file << data
    end
    
    # workaround for Ticket 67 where $/  not working yet, use 10
    def gets(sep=10)
        @block.call.next_line( sep ) #whee
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
