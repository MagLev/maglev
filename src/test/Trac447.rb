# This uses ActiveSupport's compress and decompress methods
require File.dirname(__FILE__) + '/simple'
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

original = File.read(File.dirname(__FILE__) + "/test_data/gziptest.txt")
zipped = compress(original)
unzipped = decompress(zipped)

test(unzipped, original, 'Test 1')

report

