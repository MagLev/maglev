#!/usr/bin/env maglev-ruby
# -*- ruby -*-
#
# Migrate from version 1.0.0 to version 2.0.0 in nice bite-sized chunks

# require '../lib/core_ext'

raise "No Point class installed" unless defined? Point
raise "Point VERSION #{Point::VERSION} is installed: Please install version 2.0.1" unless Point::VERSION == "2.0.1"

# Create a new-style <r,theta> Point that represents the same
# mathematical point as the old-style <tt>cartesian_point</tt>
def polar_point_from(cartesian_point)
  x = cartesian_point.x
  y = cartesian_point.y

  r = Math.sqrt((x * x) + (y * y))
  theta = Math.atan2(y, x)

  # :Point is the version 2.0.0 point, so it takes r and theta
  Point.new(r, theta)
end

# The old points of interest are in the old_points collection.  We'll set a
# chunk size and process that many, remove them from the set and commit.
CHUNK_SIZE = 3

# Since we are potentially dealing with a *very* large array, we avoid
# using the array slicing constructs, since they make copies of the data.
# Array#pop has an efficient way of truncating the array, so we use it to
# work from the end of the array down to the beginning.
count = [CHUNK_SIZE, Maglev::PERSISTENT_ROOT[:old_points].size].min

count.times do |i|
  old_point = Maglev::PERSISTENT_ROOT[:old_points].pop

  if old_point.class::VERSION == "1.0.0"
    # See the comments in migrate_100_to_200.rb for detailed comments on
    # how this works.
    new_point = polar_point_from old_point
    puts "Converting: #{old_point.inspect} to #{new_point.inspect}"
    old_point.become new_point
  else
    puts "Skip conversion of non 1.0.0 point #{old_point.inspect}"
  end
end
Maglev.commit_transaction
puts "Processed #{count} elements, #{Maglev::PERSISTENT_ROOT[:old_points].size} left"
