class IO
  include Enumerable

  ##
  # Reads the entire file specified by name as individual
  # lines, and returns those lines in an array. Lines are
  # separated by sep_string.
  #
  #  a = IO.readlines("testfile")
  #  a[0]   #=> "This is line one\n"
  def self.readlines(name, sep_string = $/)
    io = File.open( Maglev::Type.coerce_to(name, String, :to_str), 'r')
    return if io._equal?(nil)

    begin
      io.readlines(sep_string)
    ensure
      io.close
    end
  end

  ##
  # Reads all of the lines in ios, and returns them in an array.
  # Lines are separated by the optional sep_string. If sep_string
  # is nil, the rest of the stream is returned as a single record.
  # The stream must be opened for reading or an IOError will be raised.
  #
  #  f = File.new("testfile")
  #  f.readlines[0]   #=> "This is line one\n"
  def readlines(sep=$/)
    ary = []
    while line = gets(sep)
      ary << line
    end
    return ary
  end
end
