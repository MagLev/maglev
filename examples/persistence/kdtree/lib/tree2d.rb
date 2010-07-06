# This mimics the original Tree2D class used in the first kd-tree
# example, but it is implemented using the more general TreeKD.

require 'heap'
require 'treekd'

module Collections
  # Tree2D is a KD-Tree of dimension 2.
  class Tree2D < TreeKD
    # Create a Tree2D with +size+ nodes randomly located.
    def self.random(size=1_000)
      new(Array.new(size) { |i| Point2D.random("point #{i}") }, 2)
    end
    
    # Creates a new +Tree2D+ for the given points that lie in Euclidean space.
    # Points should derive from class #PointKD (extend from it).  Note that
    # there is a +Point2D+ class available for convenience.
    # If +euclidean_points+ is nil or empty, will return an empty tree.  
    # +#left+ and +#right+ may return nil, if there is no data on that side 
    # of the tree.  
    def initialize(points, depth=0)
      super(points, 2, depth)
    end
  end

  # A Point2D contains two coordinates, x and y, and may have some data
  # associated with it.  This class is also used to store latitude/
  # longitude pairs.
  class Point2D < PointKD
    MAX_SCALAR = 360.0
    MID_POINT  = MAX_SCALAR / 2.0

    # Return Point2D with random lat and lon.  Default name is :target.
    def self.random(name=:target)
      new(rand(MAX_SCALAR) - MID_POINT, rand(MAX_SCALAR) - MID_POINT, name)
    end

    attr_reader :x, :y, :data

    def initialize(x, y, data=nil)
      super([x, y], data)
    end
  end
end
