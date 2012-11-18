class Hash
  include Enumerable

  primitive_nobridge '__atkey_put', 'at:put:'
  primitive_nobridge '__at', 'at:'
  primitive_nobridge '__remove_key', 'removeKey:'
  primitive_nobridge '__to_a', 'asArray'
  primitive '__each_pair&', '_rubyEachPair:'
  primitive 'size', 'size'
  primitive 'clear', 'clear'
  primitive 'compare_by_identity?', 'isIdentityHash'
  primitive 'compare_by_identity', 'toIdentityHash'
  primitive '__head', 'head'
  primitive '__tail', 'tail'
  class_primitive_nobridge '_new', 'new'
  
  alias to_a __to_a
  alias __to_array __to_a
  alias length size
  
  def self.__new(size = 0)
    # Ignore the size argument
    _new
  end

  def default
    @_st_default
  end

  def default=(value)
    @_st_default = value
    @_st_defaultProc = nil
  end
  
  def default_proc
    @_st_defaultProc
  end

  def default_proc=(value)
    raise (TypeError, "wrong default_proc type #{value.class} (expected Proc)") if value.class != Proc and value.class != ExecBlock
    @_st_defaultProc = value
    @_st_default = nil
  end

  def to_s
    self.inspect
  end

  def dup
    hash = self.class.new
    hash.compare_by_identity if self.compare_by_identity?
    hash.default = self.default.dup if self.default
    hash.default_proc = self.default_proc.dup if self.default_proc
    
    self.each_pair { |k, v|
      hash[k] = v
    }

    hash
  end

  def each_pair(&block)
    if block_given?
      return self.__each_pair(&block)
    else
      return HashEnumerator.new(self, :each_pair)
    end
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

  def self.new(default = (default_missing = true; nil), &block)
    if !default_missing and block_given?
      raise ArgumentError, "wrong number of arguments"
    elsif !default_missing
      hash = self.__new
      hash.default = default
      return hash
    elsif block_given?
      hash = self.__new
      hash.default_proc = block
      return hash
    else
      return self.__new
    end
  end
 
  def self.try_convert(obj)
    begin
      Type.coerce_to(obj, Hash, :to_hash)
    rescue 
      nil
    end
  end
  
  def self.__from_array(array)
    raise ArgumentError, "odd number of arguments for Hash" if array.size % 2 == 1
    hash = self.new
    (array.size / 2).times { |idx| hash[array[2*idx]] = array[2*idx+1] }
    hash
  end
   
  def self.[](*args)
    hash = self.new
    if args.size == 1 and args[0].class == Array
      args[0].each { |assoc| hash[assoc[0]] = assoc[1] }
      return hash
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
      return self.__at(key)
    rescue KeyError 
      if self.default_proc == nil
        return self.default
      else
        return self.default_proc.call(self, key)
      end
    end
  end
 
  alias __atkey [] 
  def __cext_lookup(key)
    return self[key]
  end

  def store(key, value)
    if key.class == String
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
      return self.delete(key)
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
    self.each_pair { |k, v|
      str << k.inspect
      str << "=>"
      str << v.inspect
      str << ", "
    }
    str[0..(str.length - 3)] + "}"
  end

  def values
    arr = Array.new(self.size)
    idx = 0
    self.each_pair { |k, v|
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
   
  def values_at(*args)
    args.collect { |k| self[k] }
  end

  def has_value?(value)
    self.each_pair { |k, v|
      return true if value.eql?(v)
    }
   false
  end

  alias value? has_value?
  
  def has_key?(key)
    self.each_pair { |k, v|
      return true if key.eql?(k)
    }
    false
  end

  alias key? has_key?
  alias include? has_key?
  alias member? has_key?
   
  def to_hash
    self
  end
  
  def merge!(other, &block)
    other.each_pair { |k, v|
      if self.key?(k) and block_given?
        self[k] = block.call(k, self[k], v)
      else
        self[k] = v
      end
    }
  end
  
  alias update merge!
  
  def merge(other, &block)
    self.dup.merge!(other, &block)
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
    self.clear
    self.merge!(hash)
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
    self.clear
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

  def fetch(key, default = (default_missing = true; nil), &block)
    if self.key?(key)
      return self[key]
    elsif !default_missing
      return default
    elsif block_given?
      yield(key)
    else
      self.__key_error(key)
    end
  end

  def eql?(other)
    return true if other.equal?(self)
    return false unless other.class == self.class
    return false unless other.size == self.size
    
    self.each_pair { |k, v| return false unless other.key?(k) and other[k] == v }
    other.each_pair { |k, v| return false unless self.key?(k) and self[k] == v }
    true
  end

  private

  def __do_pair(block, key, value) 
    block.call(key, value)
  end

  def __key_error(key)
    raise KeyError.new("Key not found: \"#{key}\"")
  end
end

class KeyError < IndexError

end


