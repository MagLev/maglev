# Distilled from ChunkyPNG gem
#
# MagLev gets an error, MRI doesn't:
#
#   $ maglev-ruby $pbm
#   error , NoMethodError: protected method `b' for Canvas,
#                during /Users/pmclain/GemStone/dev/pbm.rb
#   ERROR 2010 , NoMethodError: protected method `b' for Canvas (MessageNotUnderstood)
#
class Canvas
  module PNGEncoding

    protected

    def a()
      Canvas.new.b()
    end

    def b()
      10
    end
  end

  include PNGEncoding
end

class Image < Canvas
  def entry()
    a()
  end
end

Image.new.entry()
