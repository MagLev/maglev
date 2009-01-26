
#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Struct < ::Struct

    # Makes it possible to access the keys directly by their name, not only
    # their index. The original Struct#values_at tries to call #to_int on the
    # arguments.
    # in Ruby < 1.9 this would lead to a quite unintuitive IndexError, while
    # in 1.9 it will say outright that it cannot convert Symbol into Integer.
    # I think this is neither useful nor expected, so we extend
    # Struct#values_at to recognize valid members of the Struct passed into
    # the method and return their values in the same order, just like
    # Hash#values_at.
    #
    # Example:
    #  Point = Struct.new(:x, :y)
    #  point = Point.new(15, 10)
    #
    #  # Old behaviour:
    #  point.values_at(0, 1)
    #  # => [15, 10]
    #  point.values_at(0..1)
    #  # => [15, 10]
    #  point.values_at(:y, :x)
    #  # => IndexError: offset 20697 too large for struct(size:2)
    #
    #  # Added new behaviour:
    #  point.values_at(:y, :x)
    #  # => [10, 15]

    def values_at(*keys)
      keys.map do |key|
        case key
        when String, Symbol
          self[key]
        else
          return super
        end
      end
    end
  end
end
