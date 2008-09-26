# Ruby Class is identically Smalltalk's Behavior
RUBY.class.primitive 'module_eval', 'evaluateString:inClass:'
 
class Class
  primitive 'alloc', 'basicNew'
  primitive 'alias_method', 'rubyAlias:from:'
  primitive 'name'
  
  def module_eval(str)
    RUBY.module_eval(str, self)
  end
  
  def new(*args)
    inst = self.alloc
    inst.initialize(*args)
    inst
  end
    
  def attr_accessor(*names)
    names.each do |n| 
        attr_reader(n)
        attr_writer(n)
    end
  end
  
  def attr_reader(*names)
    names.each do |n|
        module_eval "def #{n}; @#{n}; end"
    end
  end
  
  def attr_writer(*names)
    names.each do |n|
        module_eval "def #{n}=(v); @#{n} = v; end"
    end  
  end
  
  def protected(name)
  end
  
  def private(name)
  end

  def alias(name)
  end
  
  def const_get(name)
    name
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


  def inspect
    name
  end
  
  def to_s
    name
  end
end
