# Add VERSION 2.0.0 API compatibility methods
# This monkey-patches the class.
class Point
  def r
    Math.sqrt((@x * @x) + (@y * @y))
  end

  def theta
    Math.atan2(@y, @x)
  end
end
