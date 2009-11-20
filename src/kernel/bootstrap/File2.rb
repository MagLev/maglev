#  File2.rb , initialization of global Files after Kernel.proc available

class File
  def self.__validate_stdin_value(file)
    # used in generated code value for     $stdin = file
    cls = file.class
    if cls == File 
      return file
    end
    raise TypeError, 'expected a File'
  end

  def self.__validate_stdout_value(file)
    # used in generated code value for     $stdout = file
    unless file.equal?(nil)
      if file.respond_to?(:write)
        return file
      end
    end
    raise TypeError, 'expected a File'
  end
end

