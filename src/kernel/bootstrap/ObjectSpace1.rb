module ObjectSpace
  # resolve separate from implementation, so
  #  constant refs can be  bound at bootstrap compile time

  Finalizer = Object.__resolve_smalltalk_global(:RubyFinalizer)
  System = Object.__resolve_smalltalk_global(:System)
  Repository = Object.__resolve_smalltalk_global(:Repository)
  SystemRepository = __resolve_smalltalk_global(:SystemRepository)

  class ObjectSpaceArray < Array
    def each
      super
      size
    end
  end
end
ObjectSpace.__freeze_constants

