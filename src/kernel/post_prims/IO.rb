class IO
  def self.__cfunctions
    unless const_defined?(:CFunctions, false)
      transient_const_set(:CFunctions) do
        Module.new do
          extend FFI::Library
          ffi_lib FFI::Library::CURRENT_PROCESS

          attach_function :pipe, [:pointer], :int
        end
      end
    end
    const_get(:CFunctions)
  end

  def self.pipe
    pipefds = FFI::MemoryPointer.new :int, 2
    raise "Error creating pipes" if __cfunctions.pipe(pipefds) != 0
    # r,w = File.__fopen(pipefds[0], 'r'), File.__fopen(pipefds[1], 'w')
    r = Pipe.__send__(:new, pipefds[0], 'r', self.class)
    w = Pipe.__send__(:new, pipefds[1], 'w', self.class)
    if block_given?
      begin
        return yield(r,w)
      ensure
        r.close unless r.closed?
        w.close unless w.closed?
      end
    else
      return r,w
    end
  end

  class Pipe < Socket
    def read(*args)
      @file.read(*args)
    end

    def readpartial(*args)
      @file.readpartial(*args)
    end

    def sysread(*args)
      @file.sysread(*args)
    end

    def getc
      @file.getc
    end

    def flush
      @file.flush
    end

    def fsync
      @file.fsync
    end

    def write(*args)
      @file.write(*args)
      self
    end
    alias_method :<<, :write

    def class
      @apparant_class
    end

    private
    def self.new(fd, mode, apparant_class)
      r = self.__new_for_fd(fd)
      r.__send__(:__open_file, fd, mode)
      r.__send__(:__appear_as_class, apparant_class)
      r
    end

    def __open_file(fd, mode)
      @file = File.__fopen(fd, mode)
    end

    def __appear_as_class(klass)
      @apparant_class = klass
    end
  end
end
