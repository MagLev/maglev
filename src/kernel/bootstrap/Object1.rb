#  file Object1.rb , the bare minimum needed to create  MaglevUndefined
class Class
  primitive 'allocate', 'rubyBasicNew'
end

class Module

  primitive_nobridge '__freeze_constants', '_rubyConstantsFreeze'

end

class Object
  primitive 'freeze', 'immediateInvariant'

  # MaglevUndefined is a sentinal value used to distinguish between nil as a value passed
  # by the user and the user not passing anything for a defaulted value.  E.g.,:
  #
  #   def foo(required_param, optional_param=MaglevUndefined)
  #     if optional_param._equal?( MaglevUndefined )
  #       puts "User did not pass a value"
  #     else
  #       puts "Users passed #{optional_param} (which may be nil)"
  #     fi
  #   end
  #

  unless defined?( MaglevUndefined ) # don't change if reloading prims
    MaglevUndefined = Object.allocate
    MaglevUndefined.freeze
  end
end
Object.__freeze_constants



