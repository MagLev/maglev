#require File.expand_path('simple', File.dirname(__FILE__))

require 'zlib'
require 'stringio'


# A simple round-trip test through the file system
def zip(data, file)
  File.open(file, 'w+') do |f|
    gz_writer = Zlib::GzipWriter.new(f)
    gz_writer.write(data)
    gz_writer.close
  end
end

def unzip(file)
  new_data = ''
  File.open(file, 'r') do |f|
    gz_reader = Zlib::GzipReader.new(f)
    new_data = gz_reader.read
    gz_reader.close
  end
  new_data
end

original = "Some test data."
filename = 'test.gz'
zip(original, filename)
copy = unzip(filename)

raise "Failed: #{copy}" unless copy == original
fsize = File.size(filename)
raise "Failed to write header: expecting: 35  actual size: #{fsize}" unless fsize == 35

# data_dir = File.dirname(__FILE__) + '/test_data'
# orig_file = data_dir  + '/zlib_test_file'
# gz_file   = orig_file + '.gz'

# # Round trip with GzipWriter and GzipReader using Files
# data = "Some test data."
# deflated_data = File.new('test.gz', 'w+')
# gz_writer = Zlib::GzipWriter.new(deflated_data)
# gz_writer.write(data)
# gz_writer.close

# inflated_data = File.new('test.gz', 'r')
# gz_reader = Zlib::GzipReader.new(inflated_data)
# new_data = gz_reader.read
# gz_reader.close
# puts new_data

# # Round trip with GzipWriter and GzipReader using StringIO
# data = "Some test data."
# deflated_data = StringIO.new('', "r+")
# gz_writer = Zlib::GzipWriter.new(deflated_data)
# gz_writer.write(data)
# gz_writer.close

# x = deflated_data.string
# p x
# p x.length

# inflated_data = StringIO.new(deflated_data.string, "r+")
# gz_reader = Zlib::GzipReader.new(inflated_data)
# new_data = gz_reader.read
# gz_reader.close
# puts new_data

# some_data = "Here is some data      with         spaces"
# zipper = Zlib::Deflate.new
# compressed = zipper.deflate(some_data, Zlib::FINISH)

# unzipper = Zlib::Inflate.new
# unzipper << compressed
# processed_data = unzipper.inflate(nil)
# puts "original:       #{some_data}"
# puts "processed_data: #{processed_data}"
# raise "Failed round trip" unless processed_data == some_data



# # A test using a file created by the Mac OS X gzip utility.

# data_dir = File.dirname(__FILE__) + '/test_data'
# orig_file = data_dir  + '/zlib_test_file'
# gz_file   = orig_file + '.gz'
# orig_data = File.read(orig_file)
# File.open(gz_file) do |f|
#   gzip = Zlib::GzipReader.new(f)
#   data = gzip.read
#   raise "Fail" unless data == orig_data
# end

# def deflate(a_string, level = Zlib::BEST_SPEED)
#   z = Zlib::Deflate.new(level)
#   compressed = z.deflate(a_string, Zlib::FINISH)
#   z.close
#   puts "deflate:  #{compressed.inspect}"
#   compressed
# end

# # Uncompress compressed_string and return it
# def inflate(compressed_string)
#   zstream = Zlib::Inflate.new
#   buf = zstream.inflate(compressed_string)
#   zstream.finish
#   zstream.close
#   puts "inflate:  #{buf.inspect}"
#   buf
# end

# original = "Hello Mom"
# compressed = deflate(original)
# uncompressed = inflate(compressed)
# raise "Not the same: original: #{original}  uncompressed: #{uncompressed}" unless uncompressed == original

#report
#true
