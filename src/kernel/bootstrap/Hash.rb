class Hash

  primitive 'hash'
  primitive '_keys', 'keys'


  # Class methods
  self.class.primitive 'new'
  self.class.primitive 'new', 'new:'
  self.class.primitive 'new&', 'new:'
  # MNI: def self.[](*elements)

  # Instance Methods

  primitive '==', '='

  primitive '[]', 'at:'
  primitive '[]=', 'at:put:'

  # MNI: clear

  primitive 'default'
  primitive 'default&' , 'default:'
  primitive 'default=', 'setDefaultValue:'

  primitive 'default_proc' , 'defaultBlock'

  primitive 'delete', 'removeKey:'

  # MNI: delete_if

  primitive 'each&', 'keysAndValuesDo:'

  # MNI: each_key
  # MNI: each_pair
  # MNI: each_value
  # MNI: empty?
  # MNI: fetch

  primitive 'has_key?', 'includesKey:'

  # MNI: has_value?

  # RxINC: does the primitive work?  It was already commented out
  # primitive 'include?' 'includesKey:'
  def include?; has_key?; end  # RxINC: alias doesn't work for this one...

  # MNI index
  # MNI indexes
  # MNI incicies
  # MNI invert

  def key?; has_key?; end  # RxINC: alias doesn't work for this one...

  def keys
    set = _keys
    set.class.primitive 'to_a', 'asArray'
    set.to_a
  end

  primitive 'length', 'size'

  # MNI member?

  def merge(hash)
    dup.update(hash)
  end
  # MNI merge!
  # MNI rehash
  # MNI reject
  # MNI reject!

  def replace(hash)
    keys.each{|k| delete k}
    update(hash)
  end

  # MNI select
  # MNI shift

  primitive 'size', 'size'

  # MNI sort

  primitive  'store', 'at:put:'

  # MNI to_a
  # MNI to_hash
  # MNI to_s

  def update(hash)
    hash.each{|k,v| self[k] = v}
    self
  end

  # MNI value?
  # MNI values

  # MNI values_at

  # Overrides from Object
  #
  # RxINC: does the primitive work?
  primitive 'dup', 'copy'
  primitive 'inspect', 'printString'
  def inspect
    return "{}" if length == 0
    str = "{"
    each{|k,v| str << k.inspect; str << "=>"; str << v.inspect; str << ", "}
    str[0..(str.length - 3)] + "}"
  end

  # RxINC: Need overrides from enumerable and comparable too?
end

class Struct
end
