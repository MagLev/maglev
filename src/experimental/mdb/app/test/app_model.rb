# This file represents the application's model / view class.  This is
# required by both MagLev and MRI.
class AppModel

  # A sample view method
  def self.view_42
    42
  end

  attr_reader :x, :y
  def initialize(x, y)
    @x, @y = x, y
  end
end
