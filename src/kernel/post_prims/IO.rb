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
    r,w = File.__fopen(pipefds[0], 'r'), File.__fopen(pipefds[1], 'w')
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
end
