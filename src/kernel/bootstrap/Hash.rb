class Hash

  primitive 'hash'
#  primitive 'keys', 'keys'

  # RxINC: Hack to get around broken primitive until Allen gets back from
  # vacation
  def keys
    result = []
    each { |k,v| result << k}
    result
  end

  # Class methods
  self.class.primitive 'new'
  self.class.primitive 'new', 'new:'
  self.class.primitive 'new&', 'new:'
  # MNI: def self.[](*elements)

  # Instance Methods

  primitive '==', '='

  primitive '[]', 'at:'
  primitive '[]=', 'at:put:'

  # TODO: PERFORMANCE: Hash#clear: Implemented in ruby, needs performance boost?
  def clear
    each { |k,v| delete k }
    self
  end

  primitive 'default'
  primitive 'default&' , 'default:'
  primitive 'default=', 'setDefaultValue:'

  primitive 'default_proc' , 'defaultBlock'

  primitive 'delete', 'removeKey:'
  primitive 'delete&', 'removeKey:with:'

  def delete_if(&block)
    # RUBINIUS: This code is from rubinius core/hash.rb
    #raise LocalJumpError, "no block given" unless block_given? or empty? # TODO: uncomment

    # Do this in 2 steps, so we're not altering the structure while we walk it.
    # TODO: I'd like to write it like this:
    # select(&block).each { |k, v| delete k }
    to_del = []
    each_pair { |k, v| to_del << k if yield(k, v) }
    to_del.each { |k| delete k }
    self
  end

  primitive 'each&', 'keysAndValuesDo:'
  primitive 'each_key&', 'keysDo:'
  primitive 'each_pair&', 'keysAndValuesDo:'
  primitive 'each_value&', 'valuesDo:'

  def empty?
    size == 0
  end

  # Return a value from the hash for the given +key+.  If +key+ is not
  # found, one of the following is returned:
  # * If no other parameters passed, Raise +IndexError+.
  # * If +default+ is given, return it.
  # * If +block+ is given, return the value of calling +block+ with +key+
  # Fetch does not use any default values supplied when the hash was created.
  #
  # TODO: Need to test this, as block_given? not working properly yet...
  def fetch(key, default=Undefined, &block)
    val = _index(key, proc { default })
    puts "val: #{val}  undefined? #{val.equal?(Undefined)}"
    return val unless val.equal?(Undefined) # found it or used user default
    return block.call(key) if block_given?
    raise IndexError, "No value for #{key}"
  end

  primitive 'has_key?', 'includesKey:'

  def has_value?(val)
    each do |k,v|
      return true if v == val
    end
    false
  end

  # TODO: include?  does includesKey: work?
  # primitive 'include?' 'includesKey:'
  def include?
    has_key?
  end

  primitive '_index', 'keyAtValue:ifAbsent:'
  def index(value)
    _index(value, proc { return nil })
  end

  # MNI indexes
  # MNI incicies

  def invert
    result = {}
    each { |k,v| result[v] = k }
    result
  end

  primitive 'key?', 'includesKey:'
  primitive 'length', 'size'
  primitive 'member?', 'includesKey:'

  def merge(hash)
    dup.update(hash)
  end

  def merge!(other)
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

  primitive 'rehash', 'rebuildTable:'

  def reject(&block)
    dup.delete_if(&block)
  end

  def reject!(&block)
    # RUBINIUS
    old_size = size
    delete_if(&block)
    return nil if old_size == size
    self
  end

  def replace(hash)
    keys.each{|k| delete k}
    update(hash)
  end

  def select()
    result = []
    each { |k,v| result <<[k,v] if yield(k, v) }
    result
  end

  def shift
    return default(nil) if empty?

    # PERFORMANCE: Should be able to grab a key w/o creating the entire key set.
    # need ST help on this...
    key = keys.first
    result = [key, self[key]]
    delete key
    result
  end

  primitive 'size', 'size'

  def sort(&block)
    to_a.sort(&block)
  end

  primitive  'store', 'at:put:'

  def to_a
    select { true }
  end

  def to_hash
    self
  end

  def to_s
    to_a.join
  end

  def update(hash)
    hash.each{|k,v| self[k] = v}
    self
  end

  #alias value? has_value?
  # TODO: alias doesn't currently work for methods with '?'
  def value?(val)
    has_value? val
  end

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
  primitive 'dup', 'copy'

#  primitive 'inspect', 'printString'
  def inspect
    return "{}" if length == 0
    str = "{"
    each{|k,v| str << k.inspect; str << "=>"; str << v.inspect; str << ", "}
    str[0..(str.length - 3)] + "}"
  end

  # RxINC: Need overrides from enumerable and comparable too?
end
