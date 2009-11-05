
class Hash

  primitive 'hash' , '_hash'
  primitive 'keys', 'keys'

  primitive '__add_keys_to', '_addKeysTo:'  # used by parser

  # Class methods
  class_primitive_nobridge '__new', '_new:'

  def self.allocate
    # for spec compatibility, not used by implementation of new
    self.__st_initialized_instance
  end

  def self.new(*args)
    # this variant gets bridge methods
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
      h = self.__st_initialized_instance
      h.initialize(*args)
    end
    h
  end

  def self.new(&block)
    # block is expected to be a two-arg block { | aHash, aKey | ... }
    # subsequent variants replace just the corresponding bridge method
    h = self.__st_initialized_instance
    if block_given?
      h.initialize(&block)
    else
      h.initialize
    end
    h
  end

  def self.new
    h = self.__st_initialized_instance
    h.initialize
    h
  end

  def self.new(default_value)
    h = self.__st_initialized_instance
    h.initialize(default_value)
    h
  end

  def self.new(default_value, &block)
    # raises ArgumentError, too-many args, unless  a subclass of Hash
    #   has implemented forms of initialize to take > 1 arg
    h = self.__st_initialized_instance
    h.initialize(default_value, &block) # raises too-many args
    h
  end

  def self.__st_initialized_instance
    h = self.__new(5)
    # protects us from subclasses that don't call super in their initialize
    # method (i.e., Rack 1.0.0).  At least all of the smalltalk inst vars
    # will be initialized.
    h.default=(nil)
    h
  end

  def self.__from_elements(elements)
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
    tsize = numelem.__divide(4)
    if tsize < 5
      tsize = 5
    end
    res = self.__new(tsize)
    res.default=(nil)
    while (n < numelem)
      res[ elements[n] ] = elements[n + 1]
      n += 2
    end
    res
  end

  def self.[](*args)
    self.__from_elements(args)
  end

  def self.[](arg)
    if arg._isArray
      self.__from_elements(arg)
    elsif arg._isHash
      if self.equal?(arg.class)
        arg.dup
      else
        res = self.__new(arg.size)
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
    if other.equal?(self)
       return true
    end
    return false unless other._isHash 
    return false unless other.length.equal?(self.length) 
    # Maglev is compatible with MRI 1.8.6 
    #  by not comparing   @defaultValue == other.default  #(1.8.7 does check)
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    begin
      self.each { | k, v |
        unless other.has_key?(k)
          return false
        end
	ov = other[k]
	if v.equal?(ov)
	  # ok
	elsif ts.include?(v) || ts.include?(ov)
          if v.equal?(self) && ov.equal?(other)
            # ok
          elsif v.equal?(other) && ov.equal?(self)
            # ok
          else
            raise ArgumentError, 'recursion too complex for Hash#=='
          end
        elsif v == ov
          # ok
        else
          return false 
        end
      }
    ensure
      if added
        ts.remove(self)
      end
    end
    true
  end

  def eql?(other)
    # per specs,  does not coerce the argument
    if other.equal?(self)
      return true
    end
    return false unless other._isHash
    return false unless other.length.equal?(self.length)
    dflt = self.default
    return false unless dflt.eql?( other.default )
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    begin
      self.each { | k, v |
	ov = other[k]
        return false if ov.equal?(dflt) 
	if v.equal?(ov)
	  # ok
	elsif ts.include?(v) || ts.include?(ov)
          if v.equal?(self) && ov.equal?(other)
            # ok
          elsif v.equal?(other) && ov.equal?(self)
            # ok
          else
            raise ArgumentError, 'recursion too complex for Hash#=='
          end
        elsif v.eql?(ov)
          # ok
        else
          return false 
        end
      }
    ensure
      if added
        ts.remove(self)
      end
    end
    true
  end

  primitive_nobridge '[]', 'rubyAt:'
  primitive_nobridge '[]=', 'rubyAt:put:'

  primitive 'clear', 'removeAllKeys'

  def default(key)
    if @defaultIsBlock.equal?(true)
      @defaultValue.call(self, key)
    else
      @defaultValue
    end
  end

  def default
    if @defaultIsBlock.equal?(true)
      @defaultValue.call(nil)
    else  
      @defaultValue
    end
  end

  def default_value
    @defaultValue
  end

  primitive 'default=', 'setDefaultValue:'

  primitive 'default_proc' , 'defaultBlock'

  def delete(key, &blk)
    v = self.__delete(key)
    if v.equal?(@sentinel)
      if block_given?
        return  blk.call(key)
      else
        return nil
      end
    end
    v
  end
  primitive '__delete', 'deleteKey:' # returns @sentinel if key not found
  primitive '__delete_otherwise', 'deleteKey:otherwise:'

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
        self.__delete(to_del[n])
        n += 1
      end
    end
    self
  end

  def __call_block(arga, &blk)
    "called from Smalltalk implementation of Hash"
     blk.call(arga)
  end

  def __call_block(arga, argb, &blk)
    "called from Smalltalk implementation of Hash"
     blk.call(arga, argb)
  end

  primitive '__each&', 'eachPairDo:'
  def each(&blk)
    # this method needed so sender of eachPairDo: is in env 1
    self.__each(&blk)
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
  primitive_nobridge '__at_otherwise', 'rubyAt:otherwise:'

  def fetch(key, dflt=Undefined, &block)
    udef = Undefined
    val = self.__at_otherwise(key, udef)
    return val unless val.equal?(udef)
    if block_given?
      if dflt._not_equal?(udef)
        warn 'block supersedes default value argument'
      end
      return block.call(key) 
    end
    return dflt if dflt._not_equal?(udef)
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

  def indexes(*args)
    # deprecated, not in 1.9 ,  use values_at  for 1.9 compatibility
    args.collect { | key | self[key] }
  end

  def indices(*args)
    # deprecated, not in 1.9 ,  use values_at  for 1.9 compatibility
    args.collect { | key | self[key] }
  end

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

  primitive_nobridge '__first_pair', '_firstPair'

  def shift
    pair = self.__first_pair
    if pair.equal?(nil)
      return self.default(nil) 
    end
    key = pair[0]
    self.delete(key)
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
    args.collect { | key | self[key] }
  end

  # Overrides from Object
  #
  # RxINC: does the primitive work?

  primitive   '__basic_dup', 'rubyDup'       # use non-singleton class
  primitive   '__basic_clone', 'rubyClone'   # use singleton class

  def inspect
    return "{}" if length.equal?(0)
    str = "{"
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
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
