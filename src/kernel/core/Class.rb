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

  def inspect
    name
  end
  
  def to_s
    name
  end
end
