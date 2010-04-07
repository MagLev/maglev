module ObjectSpace
  # resolve separate from implementation, so
  #  constant refs can be  bound at bootstrap compile time

  Finalizer = Object.__resolve_smalltalk_global(:RubyFinalizer)
  System = Object.__resolve_smalltalk_global(:System)
  Repository = Object.__resolve_smalltalk_global(:Repository)
end
