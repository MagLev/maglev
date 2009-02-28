#  File2.rb , initialization of global Files after Kernel.proc available

class File
  def self._validate_stdin_value(file)
    # used in generated code value for     $stdin = file
    cls = file.class
    if cls == File || cls == PersistentFile
      return file
    end
    raise TypeError, 'expected a File'
  end

  def self._validate_stdout_value(file)
    # used in generated code value for     $stdout = file
    unless file.equal?(nil)
      if file.respond_to?(:write)
        return file
      end
    end
    raise TypeError, 'expected a File'
  end
end

STDIN = $stdin = PersistentFile.new(proc{File.stdin})
STDOUT = $stdout = PersistentFile.new(proc{File.stdout})
STDERR = $stderr = PersistentFile.new(proc{File.stderr})
$> = $stdout

