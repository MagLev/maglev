
class Class
  # Ruby Class is identically Smalltalk's Class
  # do not # include Module
  # Module already present as superclass of Class in base smalltalk image

  #  following are installed by RubyContext>>installPrimitiveBootstrap
  #    primitive_nobridge 'superclass', 'superclass' # installed in Behavior
  #  end installPrimitiveBootstrap

  class_primitive_nobridge '_rubyNew', '_rubyNew:do:'

  def inherited(a_subclass)
    # .mcz code will not invoke this during bootstrap.
    # do nothing
  end

  def self.new(superCls=Object, &blk)
    if (block_given?)
      c = _rubyNew(superCls, blk)
    else
      c = _rubyNew(superCls, nil)
    end
    superCls._ruby_inherited(c)  # See also RubyCompiler>>defineClassNamed:rubyMethod:inScope:superclass:
    c
  end

  # Allocate space for a new object of self's class.  The returned object
  # is an instance of self.
  primitive 'allocate', 'rubyBasicNew'    # init fixed instVars to remoteNil

  # base image has persistent env 1 method Class>>class

  primitive_nobridge '_include_module', 'includeRubyModule:'

  def new(*args, &block)
    # this variant gets bridge methods
    inst = self.allocate
    inst.initialize(*args, &block)
    inst
  end

  # override some bridge methods as an optimization
  def new
    inst = self.allocate
    inst.initialize
    inst
  end
  def new(a)
    inst = self.allocate
    inst.initialize(a)
    inst
  end
  def new(a,b)
    inst = self.allocate
    inst.initialize(a,b)
    inst
  end
  def new(a,b,c)
    inst = self.allocate
    inst.initialize(a,b,c)
    inst
  end
  def new(*args)
    inst = self.allocate
    inst.initialize(*args)
    inst
  end


  # In MagLev, rescue clauses use kind_of? implemented in C within the VM , to
  # determine if a the raised exception is to be handled by a specific rescue.
  # Reimplementations of === in ruby code will not be used by rescue clauses.
  #
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
      res = self._subclassOf(obj)
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

  # name inherited from Module

  def inspect
    name
  end


  def to_s
    name
  end
end
