require 'stringio'
require 'zlib'

gz_file = File.dirname(__FILE__) + '/test_data/zlib_test_file.gz'
data = StringIO.new( File.read(gz_file) )
puts Zlib::GzipReader.new(data).read
