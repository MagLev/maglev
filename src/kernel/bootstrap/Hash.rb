class Hash

  # Class methods

  class_primitive_nobridge '__allocate', '_basicNew:'
  #  tableSize arg converted to near-by prime
        # initializes @tableSize, @collisionLimit, @numCollisions, @numElements
        #  @defaultOrParent = nil , @defaultIsBlock = false
        # varying instVars initialized to RemoteNil

  def self.allocate
    # for spec compatibility, not used by this implementation
    self.__allocate(7)
  end

  def self.new(*args)
    # this variant gets bridge methods
    len = args.length
    if len <= 1
      if len._equal?(0)
        h = self.new
      else
        h = self.new(args.__at(0))
      end
    else
      # raises ArgumentError, too-many args, unless  a subclass of Hash
      #   has implemented forms of initialize to take > 1 arg
      h = self.__allocate(7)
      h.initialize(*args)
    end
    h
  end

  def self.__new(table_size)
    h = self.__allocate(table_size)
    h.initialize  # required in case initialize is reimplemented in a subclass
    h
  end

  def self.new(&block)
    # block is expected to be a two-arg block { | aHash, aKey | ... }
    # subsequent variants replace just the corresponding bridge method
    h = self.__allocate(7)
    if block_given?
      h.initialize(&block)
    else
      h.initialize
    end
    h
  end

  def self.new
    h = self.__allocate(7)
    h.initialize
    h
  end

  def self.new(default_value)
    h = self.__allocate(7)
    h.initialize(default_value)
    h
  end

  def self.new(default_value, &block)
    # raises ArgumentError, too-many args, unless  a subclass of Hash
    #   has implemented forms of initialize to take > 1 arg
    h = self.__allocate(7)
    h.initialize(default_value, &block) # raises too-many args
    h
  end

  def self.__from_elements(elements)
    numelem = elements.length
    if (numelem & 1)._equal?(1)
      if (numelem._equal?(1))
        first = elements.__at(0)
        if (first._isHash)
          return self.__atkey(first)
        end
        return Type.coerce_to(first, Hash, :to_hash)
      end
      raise ArgumentError , 'odd number of args'
    end
    n = 0
    res = self.__new(numelem)
    while (n < numelem)
      res.__atkey_put( elements.__at(n) , elements.__at(n + 1))
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
      if self._equal?(arg.class)
        arg.dup
      else
        res = self.__new(arg.size)
        res.default=(nil)
        arg.each_pair { |k, v| res.__atkey_put( k, v) }
        res
      end
    else
      Type.coerce_to(arg, Hash, :to_hash)
    end
  end

  # Instance Methods

  def initialize(*args)
    if (args.length > 1)
      raise ArgumentError, 'too many args'
    end

    # Do not call self.initialize: can lead to stack overflow from derived
    # classes (Trac 675)
    # self.initialize(args.__at(0))
    self.default=(args.__at(0))
  end

  def initialize  # zero args
    # allocation includes self.default=(nil)
    self
  end
  def initialize(one_arg)
    self.default=(one_arg)
  end
  def initialize(&block)
    self.default=(block)
  end
  def initialize(one_arg, &block)
    raise ArgumentError, 'too many args'
    self
  end

  def ==(other)
    if other._equal?(self)
       return true
    end
    return false unless other._isHash
    return false unless other.length._equal?(self.length)
    # Maglev is compatible with MRI 1.8.6
    #  by not comparing   @defaultOrParent == other.default  #(1.8.7 does check)
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    begin
      self.each_pair { | k, v |
        unless other.has_key?(k)
          return false
        end
  ov = other.__atkey(k)
  if v._equal?(ov)
    # ok
  elsif ts.include?(v) || ts.include?(ov)
          if v._equal?(self) && ov._equal?(other)
            # ok
          elsif v._equal?(other) && ov._equal?(self)
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
    if other._equal?(self)
      return true
    end
    return false unless other._isHash
    return false unless other.length._equal?(self.length)
    dflt = self.default
    return false unless dflt.eql?( other.default )
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    begin
      self.each_pair { | k, v |
  ov = other.__atkey(k)
        return false if ov._equal?(dflt)
  if v._equal?(ov)
    # ok
  elsif ts.include?(v) || ts.include?(ov)
          if v._equal?(self) && ov._equal?(other)
            # ok
          elsif v._equal?(other) && ov._equal?(self)
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

  def hash
    97633 ^ @numElements
  end

  def __tableSize
    @tableSize
  end

  primitive '__at', '_atZ:'  # prim 858 , zero based arg
                 # raises error if arg negative, or past end

  def __add_keys_to(set)  # used by parser # [
    lim = @tableSize
    lim = lim + lim
    kofs = 0
    while kofs < lim
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        set.add(k) # k,v pair
      else
        v = self.__at(kofs + 1)
        if v._not_equal?(RemoteNil)
          if v._isFixnum
            # internal collision chain
            idx = v
            begin
              ck = self.__at(idx)
              if ck._not_equal?(RemoteNil)
                set.add(ck)
              end
              idx = self.__at(idx + 2)
            end while idx._isFixnum
          else
            # a collision bucket , which is a small Hash (or a RubyCollisionBucket?)
            v.__add_keys_to(set)
          end
        end
      end
      kofs += 2
    end
  end # ]

  def __atkey(key)  # [
    kh = key.hash
    kofs = kh % @tableSize  ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
    return v
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
    if key.eql?( self.__at(idx) )
            return self.__at(idx + 1)
          end
    idx = self.__at(idx + 2)
        end while idx._isFixnum
      else
        v = v.__bucket_at(key, kh) # a collision bucket
        if v._not_equal?(RemoteNil)
          return v
        end
      end
    end
    self.default(key)  # allow for reimplem of   default
  end # ]

  alias [] __atkey

  def __bucket_at(key, khash)
    # parent is a Hash , khash is the result of key.hash
    # returns value for key, or RemoteNil if key not found
    kofs = khash % @tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          return v
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          if key.eql?( self.__at(idx) )
            return self.__at(idx + 1)
          end
          idx = self.__at(idx + 2)
        end while idx._isFixnum
      end
    end
    RemoteNil # indicates not-found
  end

  def __at_orRNil(key)
    kh = key.hash
    kofs = kh % @tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          return self.__at(kofs + 1)
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          if key.eql?( self.__at(idx) )
            return self.__at(idx + 1)
          end
          idx = self.__at(idx + 2)
        end while idx._isFixnum
      else
        return v.__bucket_at(key, kh) # a collision bucket
      end
    end
    return RemoteNil
  end

  primitive_nobridge '__at_put', '_atZ:put:'  # prim 860 , zero based arg
      # auto grow for store of 1 to 3 past end

  primitive '__varying_size', '_basicSize'
  primitive '__varying_size=', '_basicSize:'

  def __atkey_put(key, value) # [
    if key._isString
      unless key.frozen?
        key = key.dup
        key.freeze
      end
    end
    kh = key.hash
    kofs = kh % @tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          self.__at_put(kofs + 1 , value)
          return value
        else
          # convert entry to collision chain or bucket
          empty_idx = self.__varying_size
          if empty_idx <= 2014
            self.__at_put(empty_idx + 5, nil) # auto-grows
            self.__at_put(empty_idx + 3,  key)
            self.__at_put(empty_idx + 4, value)
            self.__at_put(empty_idx + 2, empty_idx + 3)
            self.__at_put(empty_idx + 0, k)
            self.__at_put(empty_idx + 1, v)
            self.__at_put(kofs,     RemoteNil )
            self.__at_put(kofs + 1, empty_idx)
          else
            bkt = Hash.__new( 7 )
            bkt.__parent=(self)
            bkt.__bucket_at_put(k, k.hash , v)
            bkt.__bucket_at_put(key, kh, value)
            self.__at_put(kofs, RemoteNil)
            self.__at_put(kofs + 1, bkt )
          end
          delta = 1
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        empty_idx = nil
        begin
          ck = self.__at(idx)
          if ck._equal?(RemoteNil)
            if empty_idx._equal?(nil)
              empty_idx = idx
            end
          elsif key.eql?( ck )
            self.__at_put(idx + 1, value)
            return value
          end
          last_idx = idx
          idx = self.__at(idx + 2)
        end while idx._isFixnum
        if empty_idx._equal?(nil)
          # did not find an empty coll chain entry
          empty_idx = self.__varying_size
          self.__at_put(last_idx + 2, empty_idx)
        end
        self.__at_put(empty_idx + 2, nil) # auto-grows
        self.__at_put(empty_idx, key)
        self.__at_put(empty_idx + 1, value)
        delta = 1
      else
        # a collision bucket
        delta = v.__bucket_at_put( key, kh, value)
      end
      nelem = @numElements + delta  # delta is zero or 1
      @numElements = nelem
      ncoll = @numCollisions + delta
      @numCollisions = ncoll
      if ncoll > @collisionLimit
        self.__rebuild( (nelem.to_f * 2).to_i )
      end
    else
      # empty entry in hash table
      self.__at_put(kofs , key)
      self.__at_put(kofs + 1, value)
      @numElements = @numElements + 1
    end
    value # return
  end # ]

  alias []= __atkey_put

  def __bucket_at_put( key, khash, value)
    # returns 1 if a new entry added to bucket,
    #         0 if key found and value replaced
    kofs = khash % @tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          self.__at_put(kofs + 1 , value)
          return 0
        else
          # convert entry to collision chain
          empty_idx = self.__varying_size
    self.__at_put(empty_idx + 5, nil) # auto-grows
    self.__at_put(empty_idx + 3,  key)
    self.__at_put(empty_idx + 4, value)
    self.__at_put(empty_idx + 2, empty_idx + 3)
    self.__at_put(empty_idx + 0, k)
    self.__at_put(empty_idx + 1, v)
    self.__at_put(kofs,     RemoteNil )
    self.__at_put(kofs + 1, empty_idx)
          return 1
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        empty_idx = nil
        begin
          ck = self.__at(idx)
          if ck._equal?(RemoteNil)
            if empty_idx._equal?(nil)
              empty_idx = idx
            end
          elsif key.eql?( ck )
            self.__at_put(idx + 1, value)
            return 0
          end
          last_idx = idx
          idx = self.__at(idx + 2)
        end while idx._isFixnum
        if empty_idx._equal?(nil)
          # did not find an empty coll chain entry
          empty_idx = self.__varying_size
          self.__at_put(last_idx + 2, empty_idx)
        end
        self.__at_put(empty_idx + 2, nil) # auto-grows
        self.__at_put(empty_idx, key)
        self.__at_put(empty_idx + 1, value)
      else
        raise 'Inconsistent Hash collision bucket'
        return 0
      end
      @numElements = @numElements + 1
      # @numCollisions  not maintained in buckets
    else
      # empty entry in hash table
      self.__at_put(kofs , key)
      self.__at_put(kofs + 1, value)
      @numElements = @numElements + 1
    end
    return 1
  end

  def __delete(key) # [
    # returns value removed, or RemoteNil
    kh = key.hash
    kofs = kh % @tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          self.__at_put(kofs + 1 , RemoteNil)
          self.__at_put(kofs + 1, RemoteNil)
          @numElements = @numElements - 1
          return v
        end
      elsif v._isFixnum
  idx = v  # internal collision chain
  begin
    ck = self.__at(idx)
    if key.eql?( ck )
      v = self.__at(idx + 1)
      self.__at_put(idx , RemoteNil)
      self.__at_put(idx + 1, RemoteNil)
      @numElements = @numElements - 1
      @numCollisions = @numCollisions - 1
      return v
    end
    idx = self.__at(idx + 2)
  end while idx._isFixnum
      else
  # a collision bucket
  val = v.__bucket_delete( key, kh )
  if val._not_equal?(RemoteNil)
    @numElements = @numElements - 1
    @numCollisions = @numCollisions - 1
  end
  return val
      end
    end
    return RemoteNil # not found
  end # ]

  def __bucket_delete( key, khash )
    kofs = khash % @tableSize  ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          self.__at_put(kofs + 1 , RemoteNil)
          self.__at_put(kofs + 1, RemoteNil)
          @numElements = @numElements - 1
          return v
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          ck = self.__at(idx)
          if key.eql?( ck )
            v = self.__at(idx + 1)
            self.__at_put(idx , RemoteNil)
            self.__at_put(idx + 1, RemoteNil)
            @numElements = @numElements - 1
            # @numCollisions not maintained in buckets
            return v
          end
          idx = self.__at(idx + 2)
        end while idx._isFixnum
      else
        raise 'Inconsistent Hash collision bucket'
      end
    end
    return RemoteNil
  end

  def delete(key, &blk)
    v = self.__delete(key)
    if v._equal?(RemoteNil)
      if block_given?
        return  blk.call(key)
      else
        return nil
      end
    end
    v
  end

  def delete(key)
    v = self.__delete(key)
    if v._equal?(RemoteNil)
      return nil
    end
    v
  end

  def __delete_ifpresent(key)
    # returns true if key was found
    self.__delete(key)._not_equal?(RemoteNil)
  end

  primitive '__fill_resize', 'fillFrom:resizeTo:with:' # args 1 is one-based

  def clear
    ts = @tableSize
    if ts > 20
      ts = 19
    end
    self.__clear(ts)
  end

  primitive '__prime_sizes', '_primeTableSize:'

  def __clear(new_size)
    arr = self.__prime_sizes(new_size)
    @collisionLimit = arr.__at(1)
    ts = arr.__at(0)
    @tableSize = ts
    two_ts = ts + ts
    self.__fill_resize(1, two_ts, RemoteNil) # one-based first arg
    @numElements = 0
    @numCollisions = 0
    self
  end

  def default(key)
    if @defaultIsBlock
      @defaultOrParent.call(self, key)
    else
      @defaultOrParent
    end
  end

  def default
    if @defaultIsBlock
      @defaultOrParent.call(nil)
    else
      @defaultOrParent
    end
  end

  def __parent
    # message only valid for collision buckets
    @defaultOrParent
  end

  def __parent=(h)
    # message only valid for collision buckets
    @defaultOrParent = h
  end

  def default=(value)
    # returns receiver
    @defaultOrParent = value
    @defaultIsBlock = value._isBlock
    self
  end

  def default_proc
    if @defaultIsBlock
      @defaultOrParent
    else
      nil
    end
  end

  def __store_defaults(value, is_block)
  end

  def delete_if(&block)
    # RUBINIUS: This code is from rubinius core/hash.rb ;  modified.
    unless @numElements._equal?(0)
      raise LocalJumpError, "no block given" unless block_given?

      # Do this in 2 steps, so we're not altering the structure while we walk it.
      to_del = []
      each_pair { |k, v| to_del << k if yield(k, v) }
      len = to_del.length
      n = 0
      while n < len
        self.__delete(to_del.__at(n))
        n += 1
      end
    end
    self
  end

  def __set_collision_limit(cl)
    # returns previous collision limit
    res = @collisionLimit
    @collisionLimit = cl
    res
  end

  primitive_nobridge '__become', '_becomeMinimalChecks:'

  def __rebuild(new_size)
    # puts "start rebuild ( "
    nhash = self.class.__new(new_size)
    nhash.default=( @defaultOrParent )
    save_cl = nhash.__set_collision_limit(Fixnum_MAX) # prevent recursive rebuild

    nhash.__become(self) # before populating new implementation
       #   so new buckets get proper parent ref
    nhash.__merge_into(self)

    self.__set_collision_limit(save_cl)
    # puts " ) end rebuild to ts #{@tableSize} from ts #{nhash.__tableSize} size #{@numElements}"
    self
  end

  def rehash
    if self.frozen?
      raise TypeError, 'rehash called on frozen instance of Hash'
    end
    self.__rebuild(@numElements)
  end

  def __merge_into(other)
    lim = @tableSize
    lim = lim + lim
    kofs = 0
    while kofs < lim
      v = self.__at(kofs + 1)
      if v._not_equal?(RemoteNil)
        k = self.__at(kofs)
        if k._not_equal?(RemoteNil)
          other.__atkey_put( k , v)
        elsif v._isFixnum
          # internal collision chain
          idx = v
          begin
            ck = self.__at(idx)
            if ck._not_equal?(RemoteNil)
              other.__atkey_put( ck , self.__at(idx + 1))
            end
            idx = self.__at(idx + 2)
          end while idx._isFixnum
        else
          # a collision bucket
          v.__merge_into(other)
        end
      end
      kofs += 2
    end
  end

  def each_pair(&block)
    lim = @tableSize
    lim = lim + lim
    kofs = 0
    while kofs < lim
      v = self.__at(kofs + 1)
      if v._not_equal?(RemoteNil)
        k = self.__at(kofs)
        if k._not_equal?(RemoteNil)
          block.call(k, v)
        elsif v._isFixnum
          # internal collision chain
    idx = v
    begin
      ck = self.__at(idx)
      if ck._not_equal?(RemoteNil)
        block.call(ck, self.__at(idx + 1) )
            end
            idx = self.__at(idx + 2)
          end while idx._isFixnum
        else
          # a collision bucket , which is a small Hash (or a RubyCollisionBucket?)
          v.each_pair(&block)
        end
      end
      kofs += 2
    end
    self
  end

  alias each each_pair

  def each_key(&block)
    lim = @tableSize
    lim = lim + lim
    kofs = 0
    while kofs < lim
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        block.call(k)
      else
        v = self.__at(kofs + 1)
        if v._not_equal?(RemoteNil)
          if v._isFixnum
            # internal collision chain
            idx = v
            begin
              ck = self.__at(idx)
              if ck._not_equal?(RemoteNil)
                block.call(ck)
              end
              idx = self.__at(idx + 2)
            end while idx._isFixnum
          else
            # a collision bucket , which is a small Hash (or a RubyCollisionBucket?)
            v.each_key(&block)
          end
        end
      end
      kofs += 2
    end
    self
  end

  def each_value(&block)
    lim = @tableSize
    lim = lim + lim
    kofs = 0
    while kofs < lim
      v = self.__at(kofs + 1)
      if v._not_equal?(RemoteNil)
        k = self.__at(kofs)
        if k._not_equal?(RemoteNil)
          block.call( v)
        elsif v._isFixnum
          # internal collision chain
          idx = v
          begin
            ck = self.__at(idx)
            if ck._not_equal?(RemoteNil)
              block.call( self.__at(idx + 1) )
            end
            idx = self.__at(idx + 2)
          end while idx._isFixnum
        else
          # a collision bucket , which is a small Hash (or a RubyCollisionBucket?)
          v.each_value(&block)
        end
      end
      kofs += 2
    end
    self
  end

  def empty?
    @numElements._equal?(0)
  end

  # Return a value from the hash for the given +key+.  If +key+ is not
  # found, one of the following is returned:
  # * If no other parameters passed, Raise +IndexError+.
  # * If +default+ is given, return it.
  # * If +block+ is given, return the value of calling +block+ with +key+
  # Fetch does not use any default values supplied when the hash was created.
  #

  def fetch(key, dflt=Undefined, &block)
    udef = Undefined
    val = self.__at_orRNil(key)
    return val unless val._equal?(RemoteNil)
    if block_given?
      if dflt._not_equal?(udef)
        warn 'block supersedes default value argument'
      end
      return block.call(key)
    end
    return dflt if dflt._not_equal?(udef)
    raise IndexError, "No value for #{key}"
  end

  def has_key?(key)
    self.__at_orRNil(key)._not_equal?(RemoteNil)
  end

  def has_value?(val)
    self.each_pair { |k,v|
      return true if v == val
    }
    false
  end

  def index(value)
    self.each_pair { |k,v|
      return k if v == value  # per specs, v is receiver
    }
    nil
  end

  def indexes(*args)
    # deprecated, not in 1.9 ,  use values_at  for 1.9 compatibility
    args.collect { | key | self.__atkey(key) }
  end

  def indices(*args)
    # deprecated, not in 1.9 ,  use values_at  for 1.9 compatibility
    args.collect { | key | self.__atkey(key) }
  end

  def invert
    # per specs, does not return inst of a subclass
    result = Hash.__new(@tableSize)
    self.each_pair { |k,v|
      result.__atkey_put( v,  k )
    }
    result
  end

  def keys
    arr = Array.new(@numElements)
    idx = 0
    self.each_key { | k |
      arr.__at_put(idx, k)
      idx += 1
    }
    arr
  end

  alias key? has_key?

  def length
    @numElements
  end

  def merge(other, &block)
    other = Type.coerce_to(other, Hash, :to_hash)
    other_siz = other.size
    my_siz = @numElements
    res_siz = ((my_siz + other_siz).to_f * 1.4 ).to_i
    if res_siz > (ts = @tableSize)  && ts < 1009
      h = self.class.__new(res_siz)
      self.__merge_into(h)
    else
      h = self.dup
    end
    h.__merge!(other, &block)
  end

  def merge!(other, &block)
    other = Type.coerce_to(other, Hash, :to_hash)
    other_siz = other.size
    my_siz = @numElements
    res_siz = ((my_siz + other_siz).to_f * 1.4 ).to_i
    if res_siz > (ts = @tableSize)  && ts < 1009
      self.__rebuild(res_siz)
    end
    self.__merge!(other, &block)
  end

  def __merge!(other, &block)
    if block_given?
      other.each_pair { |k, v|
        if self.has_key?( k)
          self.__atkey_put(k,  yield(k, self.__atkey(k), v))
        else
          self.__atkey_put(k, v)
        end
      }
    else
      other.__merge_into(self)
    end
    self
  end

  def reject(&block)
    self.dup.delete_if(&block)
  end

  def reject!(&block)
    old_size = size
    delete_if(&block)
    return nil if old_size == size
    self
  end

  def replace(hash)
    hash = Type.coerce_to(hash, Hash, :to_hash)
    self.__clear( hash.size )
    hash.__merge_into(self)
    self
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

  def __first_pair
    self.each_pair { | k ,v |
      return [ k , v ]
    }
    nil
  end

  def shift
    pair = self.__first_pair
    if pair._equal?(nil)
      return self.default(nil)
    end
    self.__delete(pair.__at(0))
    return pair
  end

  alias size length

  alias store __atkey_put

  def to_hash
    self
  end

  def to_s
    to_a.join
  end

  alias update merge!

  alias value? has_value?

  def values
    arr = Array.new(@numElements)
    idx = 0
    self.each_pair { | k, v |
      arr.__at_put(idx,  v)
      idx += 1
    }
    arr
  end

  def values_at(*args)
    args.collect { | key | self.__atkey(key) }
  end

  # Overrides from Object

  # inherit __basic_dup from Object
  # inherit __basic_clone from Object

  def __clone_buckets
    lim = @tableSize
    lim = lim + lim
    kofs = 0
    while kofs < lim
      v = self.__at(kofs + 1)
      if v._not_equal?(RemoteNil)
        k = self.__at(kofs)
        if k._not_equal?(RemoteNil)
          # a k,v pair
        elsif v._isFixnum
          # internal collision chain
        else
          # a collision bucket
          nbucket = v.clone
          nbucket.__parent=(self)
          self.__at_put(kofs + 1, nbucket )
        end
      end
      kofs += 2
    end
  end

  def dup
    nhash = self.__basic_dup
    nhash.__clone_buckets
    nhash
  end

  def clone
    nhash = self.__basic_clone
    nhash.__clone_buckets
    nhash
  end

  def inspect
    return "{}" if @numElements._equal?(0)
    str = "{"
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      str << '...}'
      return str
    end
    begin
      self.each_pair { |k, v|
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

  # additional implementation in kernel/delta/hash.rb
end
