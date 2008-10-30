RUBY.class.primitive 'module_eval', 'evaluateString:inClass:'

class Class
  # Ruby Class is identically Smalltalk's Class

  #  following are installed by RubyContext>>installPrimitiveBootstrap
  #    primitive_nobridge 'superclass', 'superclass' # installed in Behavior
  #  end installPrimitiveBootstrap

  primitive 'alloc', 'basicNew'

  # base image has persistent env 1 method Class>>class

  def new(*args)
    inst = self.alloc
    inst.initialize(*args)
    inst
  end

  def === (obj)
    obj.kind_of?(self)
  end

  primitive_nobridge '_subclassOf' , 'isSubclassOf:'
  def <= ( obj)
    self._subclassOf(obj)
  end

  #  < <= > >=  defined for other object being a Class or Module
  def < (obj)
    if (self.equal?(obj))
      res = false
    else
      res = self._subclassOf
    end
    res
  end

  def >= (obj)
    r = self < obj
    ! r
  end

  def > (obj)
    r = self <= obj
    ! r
  end

  def <=> (obj)
    if (self.equal?(obj))
      r = 0
    else
      r = (self._subclassOf(obj)) ? -1 : 1
    end
    r
  end

  def name
    @name.to_s
  end

  def inspect
    name
  end

  def to_s
    name
  end
end
