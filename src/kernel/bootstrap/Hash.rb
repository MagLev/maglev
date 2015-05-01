class Hash
  include Enumerable

  primitive_nobridge '__atkey_put', 'at:put:'
  primitive_nobridge '__at', 'at:'
  primitive_nobridge '__remove_key', 'removeKey:'
  primitive_nobridge '__to_a', 'asArray'
  primitive '__each_pair&', '_rubyEachPair:'
  primitive '__size', 'size'
  primitive '__clear', 'clear'
  primitive 'compare_by_identity?', 'isIdentityHash'
  primitive 'compare_by_identity', 'toIdentityHash'
  primitive '__head', 'head'
  primitive '__tail', 'tail'
  class_primitive_nobridge '_new', 'new'
  primitive '__rubyPrepareMarshal', 'rubyPrepareMarshal'

  alias to_a __to_a
  alias __to_array __to_a
  alias length __size
  
  def self.__new(size = 0)
    # Ignore the size argument
    self._new
  end

  def default(key=nil)
    if @_st_defaultProc == nil
      return @_st_default
    else
      return @_st_defaultProc.call(self, key)
    end
  end

  def default=(value)
    @_st_default = value
    @_st_defaultProc = nil
  end
  
  def default_proc
    @_st_defaultProc
  end

  def default_proc=(value)
    raise(TypeError, "wrong default_proc type #{value.class} (expected Proc)") if !value.kind_of?(Proc) and !value.kind_of?(ExecBlock)
    @_st_defaultProc = value
    @_st_default = nil
  end

  def to_s
    self.inspect
  end
  
  def dup
    hash = self.class.new
    hash.compare_by_identity if self.compare_by_identity?
    hash.default = self.default if self.default
    hash.default_proc = self.default_proc if self.default_proc
    
    self.each_pair { |k, v|
      hash[k] = v
    }

    hash
  end

  def clear
    self.__clear
  end
  
  def size
    self.__size
  end

  def each_pair(&block)
    if block_given?
      return self.__each_pair(&block)
    else
      return HashEnumerator.new(self, :each_pair)
    end
    return Maglev::Type.coerce_to(first, Hash, :to_hash)
  end

  alias each each_pair

  def each_key(&block)
    if block_given?
      return self.__each_pair { |k, v| yield(k) }
    else
      return HashKeyEnumerator.new(self, :each_key)
    end
  end

  def each_value(&block)
    if block_given?
      return self.__each_pair { |k, v| yield(v) }
    else
      return HashEnumerator.new(self, :each_value)
    end
  end

  def self.new(*args)
    # This variant gets bridge methods
    na = args.length

    if na <= 1
      if na == 0
        h = self.new
      else
        h = self.new(args[0])
      end
    else
      h = self.__new
      h.initialize(*args) # ArgumentError
    end
    h
  end

  def self.new
    hash = self.__new
    hash.initialize
    hash
  end

  def self.new(default)
    hash = self.__new
    hash.initialize(default)
    hash
  end
  
  def self.new(&block)
    hash = self.__new
    if block_given?
      hash.initialize(&block)
    else
      hash.initialize
    end
    hash
  end

  def self.new(default, &block)
    # Raises ArgumentError
    hash = self.__new
    hash.initialize(default, &block)
    hash
  end

  def initialize(*args, &block)
    # This variant gets bridge methods
    na = args.__size
    if na._equal?(1)
      raise ArgumentError, 'wrong number of arguments' if block_given?
      self.default=(args.__at(0))
    elsif na._equal?(0)
      if block_given?
        self.default_proc = block
      else
        # allocation includes self.default=(nil)
      end
    else
      raise ArgumentError , 'wrong number of arguments'
    end
  end

  def initialize(default, &block)
    raise ArgumentError('wrong number of arguments') if block_given?
    self.default = default
    self
  end

  def initialize(default)
    self.default = default
    self
  end

  def initialize(&block)
    self.default_proc = block if block_given?
    self
  end

  def initialize
    self
  end
     
  def self.try_convert(obj)
    begin
      Maglev::Type.coerce_to(obj, Hash, :to_hash)
    rescue 
      nil
    end
  end
  
  def self.__from_array(array)
    raise ArgumentError, "odd number of arguments for Hash" if array.size % 2 == 1
    hash = self.new
    (array.size / 2).times { |idx| hash.__atkey_put(array[2*idx], array[2*idx+1]) }
    hash
  end
   
  def self.[](*args)
    hash = self.new
    if args.size == 1 and args[0].class == Array
      args[0].each { |assoc| hash[assoc[0]] = assoc[1] }
      return hash
    elsif args.size == 1 and args[0]._isHash
      if args[0].class.eql?(self.class)
        return args[0].dup
      else
        args[0].each { |k, v| hash[k] = v }
        return hash
      end
    elsif args.size == 1
      converted = self.try_convert(args[0])
      return converted.dup if args[0].class == self.class
    end

    raise ArgumentError, "odd number of arguments for Hash" if args.size % 2 == 1
    enumerator = args.each
    while true
      begin
        key = enumerator.next
        value = enumerator.next
        hash[key] = value
      rescue StopIteration
        return hash
      end
    end
  end

  def [](key)
    begin
      return self.fetch(key)
    rescue KeyError 
      if @_st_defaultProc == nil
        return @_st_default
      else
        return @_st_defaultProc.call(self, key)
      end
    end
  end
 
  alias __atkey [] 
  def __cext_lookup(key)
    return self[key]
  end

  def store(key, value)
    if !self.compare_by_identity? and key.class == String
      return self.__atkey_put(key.dup.freeze, value)
    else
      return self.__atkey_put(key, value)
    end
  end

  alias []= store

  def delete(key)
    begin
      return self.__remove_key(key)
    rescue KeyError
      if block_given?
        return yield(key)
      else
        return nil
      end
    end
    nil
  end
 
  def __delete(key)
    begin
      return self.__remove_key(key)
    rescue KeyError
      return nil
    end
  end
   
  def __delete_ifpresent(key)
    # Return true if key was found
    begin
      self.__remove_key(key)
      return true
    rescue KeyError
      return false
    end
  end
  
  def inspect
    return "{}" if self.size == 0

    str = "{"
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      str << '...}'
      return str
    end
    begin
      counter = 0
      self.each_pair { |k, v|
          counter = counter + 1
          str << k.inspect
          str << "=>"
          str << v.inspect
          str << ", "
      }
      str = str[0..-3]
      str << "}"
    ensure
      ts.remove(self)
    end
  end

  def values
    arr = Array.new(self.size)
    idx = 0
    self.__each_pair { |k, v|
      arr.__at_put(idx, v)
      idx += 1
    }
    arr
  end
  
  def keys
    arr = Array.new(self.size)
    idx = 0
    self.each_pair { |k, v|
      arr.__at_put(idx, k)
      idx += 1
    }
    arr
  end
  
  def key(value)
    self.each_pair {|k, v|
      return k if value.eql?(v)
    }
    nil
  end
  
  def index(value)
    # TODO: warning: Hash#index is deprecated; use Hash#key
    warn("warning: Hash#index is deprecated; use Hash#key")
    key(value)
  end
   
  def values_at(*args)
    args.collect { |k| self[k] }
  end

  def __has_value?(value)
    self.__each_pair { |k, v|
      return true if value.eql?(v)
    }
   false
  end

  alias has_value? __has_value?
  alias value? __has_value?
  
  def __has_key?(key)
    begin
      self.__at(key)
      true
    rescue KeyError => e
      false
    end
    # self.__each_pair { |k, v|
    #   return true if (key.eql?(k) and !self.compare_by_identity?) or (key.equal?(k) and self.compare_by_identity?)
    # }
    # false
  end
  
  alias has_key? __has_key?
  alias key? __has_key?
  alias include? __has_key?
  alias member? __has_key?
   
  def to_hash
    self
  end
  
  def __merge!(other, &block)
    other.each_pair { |k, v|
      if self.__has_key?(k) and block_given?
        self.__atkey_put(k, block.call(k, self[k], v))
      else
        self.__atkey_put(k, v)
      end
    }
    self
  end
  
  alias merge! __merge!
  alias update __merge!
  
  def merge(other, &block)
    self.dup.__merge!(other, &block)
  end
  
  def first
    self.each_pair { |k, v|
      return [k, v]
    }
    nil
  end
  
  def shift
    pair = self.first
    self.delete(pair[0])
    pair
  end
  
  def select(&block)
    hash = self.class.new
    self.each_pair { |k, v|
      hash[k] = v if yield(k, v)
    }
    hash
  end
  
  def replace(hash)
    hash = Maglev::Type.coerce_to(hash, Hash, :to_hash)
    self.__clear
    self.__merge!(hash)
  end
  
  def empty?
    self.size == 0
  end

  def delete_if(&block)
    self.each_pair { |k, v|
      self.delete(k) if yield(k, v)
    }
    self
  end
  
  def keep_if(&block)
    self.each_pair { |k, v|
      self.delete(k) unless yield(k, v)
    }
    self
  end
   
  def reject(&block)
    self.dup.delete_if &block
  end
  
  def reject!(&block)
    changes = false
    self.each_pair { |k, v|
      if yield(k,v)
        changes = true
        self.delete(k)
      end
    }
    changes ? self : nil
  end
  
  def rehash
    assocs = self.to_a
    self.__clear
    assocs.each { |a| self[a[0]] = a[1] }
    self
  end
  
  def rassoc(obj)
    self.each_pair { |k, v|
      return [k, v] if v == obj
    }
    nil
  end
  
  def assoc(obj)
    self.each_pair { |k, v|
      return [k, v] if k == obj
    }
    nil
  end

  def invert
    hash = self.class.new
    self.each_pair { |k, v|
      hash[v] = k
    }
    hash
  end
  
  def flatten(level)
    self.to_a.flatten(level)
  end

  def flatten
    self.flatten(1)
  end

  def index(value)
    warn "warning: Hash#index is deprecated; use Hash#key"
    self.key(value)
  end

  def fetch(key, default = (default_missing = true; nil), &block)
    begin
      return self.__at(key)  
    rescue KeyError => e
      # ok, try defaults
    end 
       
    if !default_missing
      return default
    elsif block_given?
      yield(key)
    else
      self.__key_error(key)
    end
  end

  def eql?(other)
    return true if other.equal?(self)
    return false unless other._isHash
    return false unless other.size == self.size
    
    self.each_pair { |k, v| return false unless other.__has_key?(k) and other[k] == v }
    other.each_pair { |k, v| return false unless self.__has_key?(k) and self[k] == v }
    true
  end

  alias == eql?

  def hash
    97633 ^ self.size
  end

  def prepare_marshal
    self.__rubyPrepareMarshal
  end

  private

  def __do_pair(block, key, value) 
    block.call(key, value)
  end

  def __key_error(key)
    raise KeyError.new("Key not found")
  end

  def __equals(key, anotherKey)
    if self.compare_by_identity?
      return key.equal?(anotherKey)
    else
      return key.eql?(anotherKey)
    end
  end
end

class KeyError < IndexError

end


