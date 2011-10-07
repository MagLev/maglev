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

#################### Trac Info
# ID:         447
# Summary:    gzip rbs test gets "No method was found for the selector #'stringOfSize:at:'"
# Changetime: 2009-04-20 21:08:03+00:00
###

#  Test files attached
#  
#  {{{
#  $ maglev-ruby -d gzip.rb 
#  ERROR 2010, No method was found for the selector #'stringOfSize:at:' when sent to '^_.^H^X
#  }}}
#  