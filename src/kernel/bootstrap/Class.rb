class Class
  # Ruby Class is identically Smalltalk's RClass

  # do not # include Module
  # Module already present as superclass of RClass in base smalltalk image

  primitive_nobridge 'ancestors' , 'rubyAncestors'

  primitive_nobridge 'superclass', 'superClass'

  class_primitive_nobridge_env '__ruby_new', '_rubyNew', ':do:'

  def inherited(a_subclass)
    # .mcz code will not invoke this during bootstrap.
    # do nothing
  end

  def self.new(super_cls=Object, &block)
    if super_cls._not_equal?(Object)
      unless super_cls._is_a?(Class)
        raise TypeError, 'superclass arg must be a Class'
      end
    end
    if (block_given?)
      c = __ruby_new(super_cls, block)
    else
      c = __ruby_new(super_cls, nil)
    end
    super_cls.__ruby_inherited(c)  # See also RubyCompiler>>defineClassNamed:rubyMethod:inScope:superclass:
    c
  end

  # Allocate space for a new object of self's class.  The returned object
  # is an instance of self.
  primitive 'allocate', 'rubyBasicNew'    # init fixed instVars to remoteNil

  # base image has persistent env 1 method Class>>class

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
    obj._kind_of?(self)
  end

  #  < <= > >=  inherited from Module

  primitive_nobridge 'included_modules' , 'rubyIncludedModules'

  primitive 'inspect', '_rubyInspect'
  primitive 'to_s', '_rubyInspect'

  # Controls whether instances of receiver are persistable.  If the flag is
  # +true+, then receiver is marked to allow instances to be persisted.
  #
  # If the flag is +false+, then receiver is marked to disallow instances
  # to be persisted.  If reciever was previously marked to allow
  # persistable instances, and instances have been committed to the
  # repository, the call will succeed, and all further instances will not
  # be committable.  The previously persisted instances remain in the
  # repository (and will hold a reference to receiver) until they are
  # cleared by the program.
  #
  # Raises +NotPersistableException+ if reciever is not persistable.
  #
  def maglev_instances_persistable=(flag=true)
    unless self.maglev_persistable?
      raise Maglev::NotPersistableException
    end
    self.__set_instances_persistent(flag)
  end
  primitive_nobridge '__set_instances_persistent', '_setInstancesPersistent:'

  primitive_nobridge '__set_protection_classmethods*', 'setProtection:classmethods:'

  # Returns +true+ if instances of receiver are allowed to be
  # persisted. Returns +false+ otherwise.
  primitive_nobridge 'maglev_instances_persistable?', '_instancesPersistent'
end
