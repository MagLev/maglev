# A Two-Dimensional point, with Cartesian Coordinates (<x,y>).
class Point
  VERSION = "1.0.0"

  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def distance_to(other)
    dx = x - other.x
    dy = y - other.y
    Math.sqrt((dx * dx) + (dy * dy))
  end
end
