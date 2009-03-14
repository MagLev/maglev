
class IdentityHash
  # a dictionary similar to a Hash but using identity comparision,
  #   i.e.  equal?()  ,  to compare keys.

  # Class methods
  class_primitive_nobridge '_new', 'new'

  def self.new
    # args are ignored
    _new
  end

  def self.from_hash(a_hash)
    ih = self._new 
    a_hash.each { | k,v |
      ih[k] = v
    }
    ih
  end

  def ==(other)
    if (other.class.equal?(self.class))
      if (other.equal?(self))
        return true
      end
      unless other.length.equal?(self.length)
        return false
      end
      each { |k,v|
         unless other[k] == v
           return false
         end
      }
      true
    else
      false
    end
  end

  primitive_nobridge '_at_otherwise', 'at:otherwise:'
  def [](key)
    # returns the value for key, or nil if key not found
    _at_otherwise(key, nil)
  end

  primitive_nobridge '[]=', 'at:put:'

  primitive 'clear', 'removeAllKeys'

  primitive 'delete', 'removeKey:'

  primitive 'each&', 'keysAndValuesDo:'
  primitive 'each_key&', 'keysDo:'
  primitive 'each_pair&', 'keysAndValuesDo:'
  primitive 'each_value&', 'valuesDo:'

  def empty?
    size.equal?(0)
  end

  primitive 'has_key?', 'includesKey:'

  def has_value?(val)
    each do |k,v|
      return true if val == v
    end
    false
  end

  primitive 'index', 'keyAtValue:'

  primitive 'key?', 'includesKey:'
  primitive 'length', 'size'

  primitive 'rehash', 'rebuildTable:'

  primitive 'size', 'size'
  primitive  'store', 'at:put:'

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
  primitive 'dup', 'copy'

  def inspect(touchedSet=nil)
    return "{}" if length.equal?(0)
    str = "{"
    if (touchedSet.equal?(nil))
      touchedSet = IdentitySet.new
    else
      if (touchedSet._includes(self))
        str << '...}'
        return str
      end
    end
    touchedSet << self
    each {|k,v|
          str << k.inspect(touchedSet)
          str << "=>"
          str << v.inspect(touchedSet)
          str << ", "
         }
    str[0..(str.length - 3)] + "}"
  end

end
