

# This is a global sentinal object representing an undefined object.  This
# is used to distinguish the user passing nil vs not passing anything for
# default parameters.
#
# TODO: This is in Hash.rb for right now, as I can't seem to call it in
# ../kernel.rb, nor in Globals.rb, nor in Object.rb. When I figure out the
# bootstrapping sequence, I can put it in the right spot.
Undefined = Object.new

class Hash

  primitive 'hash'
  primitive 'keys', 'keys'

  # Class methods
  class_primitive_nobridge 'new', 'new:'
  class_primitive_nobridge 'new&', 'new:'
  class_primitive 'new'

  def self.[](*elements)
    numelem = elements.length
    if ((numelem & 1) != 0)
      if (numelem.equal?(1) )
        first = elements[0]
        if (first._isHash) 
          return first.dup
        end
      end
      raise ArgumentError , 'odd number of args'
    end
    n = 0
    res = self.new
    while (n < numelem) 
      res[ elements[n] ] = elements[n + 1]
      n += 2
    end
    res
  end

  def self.name
    # override Smalltalk name
    'Hash'
  end

  # Instance Methods

  def ==(other)
    unless other._isHash
      other = other.to_hash
    end
    unless other.length.equal?(self.length)
      return false
    end
    unless other.default == self.default
      return false
    end 
    each { |k,v| 
       unless other[k] == v 
         return false 
       end
    }
    true
  end

  primitive_nobridge '[]', 'at:'
  primitive_nobridge '[]=', 'at:put:'

  primitive 'clear', 'removeAllKeys'

  primitive_nobridge 'default&' , 'default:'
  primitive 'default'

  primitive 'default=', 'setDefaultValue:'

  primitive 'default_proc' , 'defaultBlock'

  primitive_nobridge 'delete&', 'removeKey:with:'
  primitive 'delete', 'removeKey:'

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
  primitive_nobridge '_atIfAbsent', 'at:ifAbsent:'
  def fetch(key, dflt=Undefined, &block)
    val = _atIfAbsent(key, proc { dflt })
    puts "========= 2 VAL: #{val}"
    return val unless val.equal?(Undefined)
# TODO: block_given? does not work, so this is commented out until it does work.
#    return block.call(key) if block_given?
    raise IndexError, "No value for #{key}"
  end

  primitive 'has_key?', 'includesKey:'

  def has_value?(val)
    each do |k,v|
      return true if val == v
    end
    false
  end

  primitive 'include?', 'includesKey:'

  primitive_nobridge '_index', 'keyAtValue:ifAbsent:'
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
    each { |k,v| result << [k,v] if yield(k, v) }
    result
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

  def sort(&block)
    to_a.sort(&block)
  end

  primitive  'store', 'at:put:'

  def to_a
    result = []
    each { |k,v| result << [k,v] }
    result
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
    return "{}" if length.equal?(0)
    str = "{"
    each{|k,v| str << k.inspect; str << "=>"; str << v.inspect; str << ", "}
    str[0..(str.length - 3)] + "}"
  end

  # RxINC: Need overrides from enumerable and comparable too?
end
