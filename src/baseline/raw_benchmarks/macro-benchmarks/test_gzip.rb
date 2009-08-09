# This uses ActiveSupport's compress and decompress methods

require 'zlib'
require 'stringio'
 
class Stream < StringIO
  def close
    rewind
  end
end
 
def decompress(source)
  Zlib::GzipReader.new(StringIO.new(source)).read
end
 
def compress(source)
  output = Stream.new
  gz = Zlib::GzipWriter.new(output)
  gz.write(source)
  gz.close
  output.string
end

[100].map do |n|
  n.times do
    original = File.read("holmes.txt")
    zipped = compress(original)
    unzipped = decompress(zipped)
  end
end
