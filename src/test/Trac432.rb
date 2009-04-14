require 'zlib'
require 'stringio'

# This test uses StringIO as the IO object (just like Ruby Gems does).
def deflate(data)
  deflated_data = StringIO.new('', "r+")
  gz_writer = Zlib::GzipWriter.new(deflated_data)
  gz_writer.write(data)
  gz_writer.close
  deflated_data.string
end

def inflate(data)
  inflated_data = StringIO.new(data, "r")
  gz_reader = Zlib::GzipReader.new(inflated_data)
  new_data = gz_reader.read
  gz_reader.close
  new_data
end


# These are captures of the compressed data string produced by MRI and by
# MagLev
mri_data = "\037\213\b\000\177\v\345I\000\003\v\316\317MU(I-.QHI,I\324\003\000\246\310.%\017\000\000\000"
ml_data  = "\037\213\b\030\000\000\000\000\000\003\000\000\v\316\317MU(I-.QHI,I\324\003\000\246\310.%\017\000\000\000"

data = "Some test data."

# MRI can inflate either mri_data or ml_data
# MagLev can inflate neither.
[mri_data, ml_data].each do |d|
  puts "inflating: #{d.inspect}"
  inflated = inflate(d)  # "Some test data."
  puts "inflated data: #{inflated}"
  puts
end
