
class Hash

  primitive 'hash' , '_hash'
  primitive 'keys', 'keys'

  primitive '_add_keys_to', '_addKeysTo:'

  # Class methods
  class_primitive_nobridge '_new', '_new:'

  def self.new(*args)
    # first variant gets bridge methods
    len = args.length
    if len <= 1
      if len.equal?(0)
        h = self.new
      else
        h = self.new(args[0])
      end
    else
      # raises ArgumentError, too-many args, unless  a subclass of Hash
      #   has implemented forms of initialize to take > 1 arg
      h = self._st_initialized_instance
      h.initialize(*args)
    end
    h
  end

  def self.new(&block)
    # subsequent variants replace just the corresponding bridge method
    h = self._st_initialized_instance
    if block_given?
      h.initialize(&block)
    else
      h.initialize
    end
    h
  end

  def self.new
    h = self._st_initialized_instance
    h.initialize
    h
  end

  def self.new(default_value)
    h = self._st_initialized_instance
    h.initialize(default_value)
    h
  end

  def self.new(default_value, &block)
    # raises ArgumentError, too-many args, unless  a subclass of Hash
    #   has implemented forms of initialize to take > 1 arg
    h = self._st_initialized_instance
    h.initialize(default_value, &block) # raises too-many args
    h
  end

  def self._st_initialized_instance
    h = self._new(5)
    # protects us from subclasses that don't call super in their initialize
    # method (i.e., Rack 1.0.0).  At least all of the smalltalk inst vars
    # will be initialized.
    h.default=(nil)
    h
  end

  def self._from_elements(elements)
    numelem = elements.length
    if !((numelem & 1).equal?(0))
      if (numelem.equal?(1))
        first = elements[0]
        if (first._isHash)
          return self[first]
        end
        return Type.coerce_to(first, Hash, :to_hash)
      end
      raise ArgumentError , 'odd number of args'
    end
    n = 0
    tsize = numelem / 4
    if tsize < 5
      tsize = 5
    end
    res = self._new(tsize)
    res.default=(nil)
    while (n < numelem)
      res[ elements[n] ] = elements[n + 1]
      n += 2
    end
    res
  end

  def self.[](*args)
    self._from_elements(args)
  end

  def self.[](arg)
    if arg._isArray
      self._from_elements(arg)
    elsif arg._isHash
      if self.equal?(arg.class)
        arg.dup
      else
        res = self._new(arg.size)
        res.default=(nil)
        arg.each_pair { |k, v| res[k] = v }
        res
      end
    else
      Type.coerce_to(arg, Hash, :to_hash)
    end
  end

  # Instance Methods

  # initialize is only used to issue too-many args errors for new
  def initialize(*args)
    if (args.length > 1)
      raise ArgumentError, 'too many args'
    end
    self
  end
  def initialize  # zero args
    self.default=(nil)
    self
  end
  def initialize(one_arg)
    self.default=(one_arg)
    self
  end
  def initialize(&block)
    self.default=(block)
    self
  end
  def initialize(one_arg, &block)
    raise ArgumentError, 'too many args'
    self
  end

  def ==(other)
    if (other._isHash)
      if (other.equal?(self))
        return true
      end
    else
      return false unless other.respond_to? :to_hash
      other = other.to_hash
    end
    unless other.length.equal?(self.length)
      return false
    end
    unless other.default == self.default
      return false
    end
    each { |k,v|
       unless other[k] == v	# TODO handle recursive refs
         return false
       end
    }
    true
  end

  def eql?(other)
    # per specs,  does not coerce the argument
    if other.equal?(self)
      return true
    end
    unless other._isHash
      return false
    end
    unless other.length.equal?(self.length)
      return false
    end
    unless other.default == self.default
      return false
    end
    each { |k,v|
       unless other[k] == v	# TODO handle recursive refs
         return false
       end
    }
    true
  end

  primitive_nobridge '[]', 'rubyAt:'
  primitive_nobridge '[]=', 'rubyAt:put:'

  primitive 'clear', 'removeAllKeys'

  def _call_default_block(arg)
    # intended to be called from Smalltalk code only
    @defaultValue.call(arg)
  end

  def default(key=nil)
    if @defaultIsBlock.equal?(true)
      @defaultValue.call(key)
    else
      @defaultValue
    end
  end

  primitive 'default=', 'setDefaultValue:'

  primitive 'default_proc' , 'defaultBlock'

  def delete(key, &blk)
    v = self._delete(key)
    if block_given?
      if v.equal?(@sentinel)
        return  blk.call(key)
      end
    end
    v
  end
  primitive '_delete', 'removeKey:'

  def delete_if(&block)
    # RUBINIUS: This code is from rubinius core/hash.rb ;  modified.
    unless size.equal?(0)
      raise LocalJumpError, "no block given" unless block_given?

      # Do this in 2 steps, so we're not altering the structure while we walk it.
      to_del = []
      each_pair { |k, v| to_del << k if yield(k, v) }
      len = to_del.length
      n = 0
      while n < len
        self.delete(to_del[n])
        n += 1
      end
    end
    self
  end

  def _call_block(arga, &blk)
    "to be called from smalltalk"
     blk.call(arga)
  end

  def _call_block(arga, argb, &blk)
    "to be called from smalltalk"
     blk.call(arga, argb)
  end

  primitive '_each&', 'eachPairDo:'
  def each(&blk)
    # this method needed so sender of eachPairDo: is in env 1
    self._each(&blk)
  end

  primitive 'each_key&', 'eachKeyDo:'
  primitive 'each_pair&', 'eachPairDo:'
  primitive 'each_value&', 'eachValueDo:'

  def empty?
    size.equal?(0)
  end

  # Return a value from the hash for the given +key+.  If +key+ is not
  # found, one of the following is returned:
  # * If no other parameters passed, Raise +IndexError+.
  # * If +default+ is given, return it.
  # * If +block+ is given, return the value of calling +block+ with +key+
  # Fetch does not use any default values supplied when the hash was created.
  #
  primitive_nobridge '_at_otherwise', 'rubyAt:otherwise:'

  def fetch(key, dflt=Undefined, &block)
    val = _at_otherwise(key, dflt)
    return val unless val.equal?(Undefined)
    return block.call(key) if block_given?
    raise IndexError, "No value for #{key}"
  end

  primitive 'has_key?', 'includesKey:'

  def has_value?(val)
    each do |k,v|
      return true if val == v
    end
    false
  end

  primitive 'index', 'keyAtValue:'

  # MNI indexes
  # MNI incicies

  def invert
    result = {}
    each { |k,v| result[v] = k }
    result
  end

  primitive 'key?', 'includesKey:'
  primitive 'length', 'size'

  def merge(hash, &block)
    dup.update(hash, &block)
  end

  def merge!(other, &block)
    # RUBINIUS: From core/hash.rb
    other = Type.coerce_to(other, Hash, :to_hash)
    other.each_pair do |k, v|
      if block_given? and self.key? k
        self[k] = yield(k, self[k], v)
      else
        self[k] = v
      end
    end
    self
  end

  primitive 'rehash', 'rehash'

  def reject(&block)
    self.dup.delete_if(&block)
  end

  def reject!(&block)
    # RUBINIUS
    old_size = size
    delete_if(&block)
    return nil if old_size == size
    self
  end

  def replace(hash)
    keys.each{ |k| delete k}
    update(hash)
  end

  def select(&block)
    res = []
    each_pair { |k, v|
       if yield(k,v)
         res << [k,v]
       end
    }
    res
  end

  primitive_nobridge '_firstPair'

  def shift
    pair = self._firstPair
    return nil if  pair.equal?(nil)
    key = pair[0]
    delete key
    return pair
  end

  primitive 'size', 'size'
  primitive  'store', 'rubyAt:put:'

  def to_hash
    self
  end

  def to_s
    to_a.join
  end

  alias update merge!

  alias value? has_value?

  def values
    result = []
    each { |k,v| result << v }
    result
  end

  def values_at(*args)
    # RUBINIUS
    # TODO There is a maglev bug in Array#collect in dealing with nil
    # values from the block, so this impl exhibits the underlying bug.
    args.collect { |key| self[key] }
  end

  # Overrides from Object
  #
  # RxINC: does the primitive work?

  primitive   '_basic_dup', 'rubyDup'       # use non-singleton class
  primitive   '_basic_clone', 'rubyClone'   # use singleton class

  def inspect
    return "{}" if length.equal?(0)
    str = "{"
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
    unless added
      str << '...}'
      return str
    end
    begin
      each { |k, v|
          s = str
          s << k.inspect
          s << "=>"
          s << v.inspect
          s << ", "
         }
      str[0..(str.length - 3)] + "}"
    ensure
      ts.remove(self)
    end
  end

end
