class String
  # String contains reimplementations (for performance)
  # of all of the interesting methods in Comparable and Enumerable.
  # Comparable and Enumerable are included here only so that
  # String will inherit any methods added to Comparable and Enumerable
  #  by applications' code.

  include Comparable
  include Enumerable
end
