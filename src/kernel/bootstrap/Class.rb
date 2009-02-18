
class Class
  # Ruby Class is identically Smalltalk's Class
  # do not # include Module
  # Module already present as superclass of Class in base smalltalk image

  #  following are installed by RubyContext>>installPrimitiveBootstrap
  #    primitive_nobridge 'superclass', 'superclass' # installed in Behavior
  #  end installPrimitiveBootstrap

  class_primitive_nobridge '_rubyNew', '_rubyNew:do:'

  def self.new(superCls=Object, &blk)
    if (block_given?)
      c = _rubyNew(superCls, blk)
    else
      c = _rubyNew(superCls, nil)
    end
    superCls.inherited(c)  # See also RubyCompiler>>defineClassNamed:rubyMethod:inScope:superclass:
    c
  end

  # Allocate space for a new object of self's class.  The returned object
  # is an instance of self.
  primitive 'allocate', 'rubyBasicNew'    # init fixed instVars to remoteNil

  # base image has persistent env 1 method Class>>class

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

  primitive_nobridge 'include', 'includeRubyModule:'

  def new(*args)
    inst = self.allocate
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

  # name inherited from Module

  def inspect(touchedSet=nil)
    name
  end

    def instance_eval(*args)
      # bridge methods would interfere with VcGlobals logic
      raise ArgumentError, 'wrong number of args'
    end

    def instance_eval(str)
      string = Type.coerce_to(str, String, :to_str)
      vcgl = [ self._getRubyVcGlobal(0x20),
               self._getRubyVcGlobal(0x21) ]
      res = self.class._instance_eval(string, vcgl)
      vcgl[0]._storeRubyVcGlobal(0x20)
      vcgl[1]._storeRubyVcGlobal(0x21)
      res
    end

    def instance_eval(str, file=nil)
      string = Type.coerce_to(str, String, :to_str)
      vcgl = [ self._getRubyVcGlobal(0x20),
               self._getRubyVcGlobal(0x21) ]
      res = self.class._instance_eval(string, vcgl)
      vcgl[0]._storeRubyVcGlobal(0x20)
      vcgl[1]._storeRubyVcGlobal(0x21)
      res
    end

    def instance_eval(str, file=nil, line=nil)
      # TODO: Object#instance_eval: handle file and line params
      string = Type.coerce_to(str, String, :to_str)
      vcgl = [ self._getRubyVcGlobal(0x20),
               self._getRubyVcGlobal(0x21) ]
      res = self.class._instance_eval(string, vcgl)
      vcgl[0]._storeRubyVcGlobal(0x20)
      vcgl[1]._storeRubyVcGlobal(0x21)
      res
    end

    primitive_nobridge 'instance_eval&' , 'rubyMetaEval:'

  def to_s
    name
  end
end
