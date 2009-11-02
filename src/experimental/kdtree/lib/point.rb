# A Point contains a measure along one or more axis.  This class just
# supports two axes.
class Point
  attr_reader :x, :y, :data

  def initialize(x, y, data)
    @x, @y, @data = x, y, data
  end

  # Return the coordinate along the given axis (0 or 1)
  def [](index)
    if index == 0
      @x
    elsif index == 1
      @y
    else
      raise ArgumentError, "#{index} is out of range.  Should be 0 or 1"
    end
  end

  def to_s
    "Point<#{@x}, #{@y}> #{@data}"
  end

  def eql?(other)
    @x.eql?(other.x) and @y.eql?(other.y) and @data.eql?(other.data)
  end

  # TODO: hash_code
end
