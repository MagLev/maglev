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

  # MNI: clear: TODO: PERFORMANCE: Implemented in ruby, needs performance boost?
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
  # MNI: fetch

  primitive 'has_key?', 'includesKey:'

  # MNI: has_value?

  # TODO: include?  does includesKey: work?
  # primitive 'include?' 'includesKey:'
  def include?
    has_key?
  end

  # MNI index
  # MNI indexes
  # MNI incicies
  # MNI invert

  def key?; has_key?; end  # RxINC: alias doesn't work for this one...

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
