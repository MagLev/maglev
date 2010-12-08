# A Two-Dimensional point, with Polar Coordinates (<r,theta>).
class Point
  VERSION = "2.0.0"

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
end
