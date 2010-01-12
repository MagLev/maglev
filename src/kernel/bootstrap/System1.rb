module Maglev
  # resolve separate from implementation, so 
  #  dynamic constant definitions, if any, will work
  System = Object._resolve_smalltalk_global(:System)
end
