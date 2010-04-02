module Maglev
  # resolve separate from implementation, so 
  #  constant refs can be  bound at bootstrap compile time

  System = Object.__resolve_smalltalk_global(:System)
end
