require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  def index
    return unless request.post?
    @inspection = h(request.params.pretty_inspect)
    tempfile, filename, @type =
      request[:file].values_at(:tempfile, :filename, :type)
    @extname, @basename = File.extname(filename), File.basename(filename)
    @file_size = tempfile.size

    FileUtils.move(tempfile.path, Ramaze::Global.public_root/@basename)

    @is_image = @type.split('/').first == 'image'
  end
end

Ramaze.start