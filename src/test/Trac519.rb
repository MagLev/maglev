# Test days until Christmas 2009. Should report negative days afterwards.

# The original test case: Passes if there is no exception
require 'date'
puts "#{Date.new(2009,12,25) - Date.today} days until Christmas, 2009"

# Other test cases

class D
  class << self; alias_method :new!, :new end

  # NOTE: four parameters gets us out of the default selector set.  There
  # is an "extra" selector for 'civil'
  def self.civil(y=-4712, m=1, d=1, sg=:italy)
  end

  # The bug is that only the standard bridge methods are getting aliased.
  # Since civil has four parameters, the selector civil:::: is NOT getting
  # aliased, so we can't find the method.
  class << self; alias_method :new, :civil end
end

D.new(4712, 1, 1, :x)
D.new

