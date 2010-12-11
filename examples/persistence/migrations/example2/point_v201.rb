# A Two-Dimensional point, with Polar Coordinates (<r,theta>).
# This version also has 1.0.0 compatibility methods to support
# both versions of the class.
class Point
  VERSION = "2.0.1"

  attr_reader :r, :theta

  def initialize(r, theta)
    @r = r
    @theta = theta
  end

  def distance_to(other)
    sum_r_sq = (@r * @r) + (other.r * other.r)
    two_r_diff_cosines = 2 * @r * other.r * Math.cos(@theta - other.theta)
    Math.sqrt(sum_r_sq - two_r_diff_cosines)
  end

  # Support the VERSION 1.0.0 API
  def x
    r * Math.cos(theta)
  end

  # Support the VERSION 1.0.0 API
  def y
    r * Math.sin(theta)
  end

end
