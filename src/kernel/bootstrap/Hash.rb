class Hash

  # Class methods 

  class_primitive_nobridge '__allocate', '_basicNew:'
  #  tableSize arg converted to near-by prime
        # initializes @tableSize, @collisionLimit, @numCollisions, @numElements
        #  @defaultOrParent = nil , @defaultIsBlock = false
        # varying instVars initialized to RemoteNil

  primitive   '__basic_dup', '_rubyBasicDup'      # use non-singleton class

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
    first = elements.__at(0)
    if first._isArray     # changes for 1.8.7
      res = self.__from_array_of_pairs(elements)
    else
      numelem = elements.length
      if (numelem & 1)._equal?(1)
  if numelem._equal?(1)
    if first._isHash
      return self.__atkey(first)
    end
    return Maglev::Type.coerce_to(first, Hash, :to_hash)
  end
  raise ArgumentError , 'odd number of args'
      end
      n = 0
      res = self.__new(numelem)
      while n < numelem
        res.__atkey_put( elements.__at(n) , elements.__at(n + 1))
        n += 2
      end
    end
    res
  end

  def self.__from_array_of_pairs(elements)  # added for 1.8.7
    # handle an array of pairs, skipping elements that are not arrays of size 2
    n = 0
    numelem = elements.__size
    res = self.__new(numelem)
    while n < numelem
      pair = nil
      begin
        pair = Maglev::Type.coerce_to( elements.__at(n), Array, :to_ary )
      rescue
        # ignore coercion errors
      end
      if pair._not_equal?(nil)
        p_size = pair.__size
        if p_size <= 2 && p_size._not_equal?( 0 )
          res.__atkey_put( pair.__at(0), pair.__at(1) )
        end
      end
      n += 1
    end
    res
  end

  def self.__from_array(arr)
    # used by Array#__as_hash, from generated code , added for 1.8.7
    lim = arr.__size
    res = self.__new( lim.__divide(2) )
    n = 0
    while n < lim
      res.__atkey_put( arr.__at(n), arr.__at(n + 1) )
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
      Maglev::Type.coerce_to(arg, Hash, :to_hash)
    end
  end

  # Instance Methods

  def initialize(*args, &block)
    na = args.__size
    if na._equal?(1)
      raise ArgumentError, 'too many args' if block_given?
      self.default=(args.__at(0))
    elsif na._equal?(0)
      if block_given?
        self.default=(block)
      else
        # allocation includes self.default=(nil)
      end
    else
      raise ArgumentError , 'too many args'
    end
    self
  end

  def initialize(obj, &block)
    raise ArgumentError, 'too many args' if block_given?
    self.default=(obj)
  end

  def initialize(obj)
    self.default=(obj)
  end

  def initialize(&block)
    self.default=(block)
  end

  def initialize  # zero args
    # allocation includes self.default=(nil)
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
    97633 ^ @_st_numElements
  end

  def __tableSize
    @_st_tableSize
  end

  primitive '__at', '_atZ:'  # prim 858 , zero based arg
                 # raises error if arg negative, or past end

  def __add_keys_to(set)  # used by parser # [
    lim = @_st_tableSize
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
            # a collision bucket , which is a small Hash 
            v.__add_keys_to(set)
          end
        end
      end
      kofs += 2
    end
  end # ]

  def __atkey(key)  # [
    kh = key.hash
    kofs = kh % @_st_tableSize  ; kofs += kofs
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

  def __cext_lookup(key)  # [
    # used by implementation of rb_hash_lookup() in C extension API
    kh = key.hash
    kofs = kh % @_st_tableSize  ; kofs += kofs
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
    nil  # don't use self.default
  end # ]

  def __bucket_at(key, khash)
    # parent is a Hash , khash is the result of key.hash
    # returns value for key, or RemoteNil if key not found
    kofs = khash % @_st_tableSize ; kofs += kofs
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
    kofs = kh % @_st_tableSize ; kofs += kofs
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
      # auto grow up to 6 past end
  primitive_nobridge '__at_put', '_atZ:putKey:value:'  # prim 892 , zero based arg
      # auto grow up to 6 past end

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
    kofs = kh % @_st_tableSize ; kofs += kofs
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
            # store starting at empty_idx: k,v, empty_idx + 3, key, value, nil
            #    nil is the chain terminator
            self.__at_put(empty_idx + 4, value, nil)  # auto grows
            self.__at_put(empty_idx + 2, empty_idx + 3, key)
            self.__at_put(empty_idx + 0, k, v)
            # store starting at kofs:  empty_idx, RemoteNil
            self.__at_put(kofs,     RemoteNil, empty_idx )
          else
            bkt = Hash.__new( 7 )
            bkt.__parent=(self)
            bkt.__bucket_at_put(k, k.hash , v)
            bkt.__bucket_at_put(key, kh, value)
            self.__at_put(kofs, RemoteNil, bkt)
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
          self.__at_put(empty_idx + 2, nil) # auto-grow, chain terminator, fix 768
          self.__at_put(last_idx + 2, empty_idx)
        end
        self.__at_put(empty_idx, key, value )
        delta = 1
      else
        # a collision bucket
        delta = v.__bucket_at_put( key, kh, value)
      end
      nelem = @_st_numElements + delta  # delta is zero or 1
      @_st_numElements = nelem
      ncoll = @_st_numCollisions + delta
      @_st_numCollisions = ncoll
      if ncoll > @_st_collisionLimit
        self.__rebuild( (nelem.to_f * 2).to_i )
      end
    else
      # empty entry in hash table
      self.__at_put(kofs , key, value)
      @_st_numElements = @_st_numElements + 1
    end
    value # return
  end # ]

  alias []= __atkey_put

  def __bucket_at_put( key, khash, value)
    # returns 1 if a new entry added to bucket,
    #         0 if key found and value replaced
    kofs = khash % @_st_tableSize ; kofs += kofs
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
          # store starting at empty_idx: k,v, empty_idx + 3, key, value, nil
          self.__at_put(empty_idx + 4, value, nil )
          self.__at_put(empty_idx + 2, empty_idx + 3, key)
          self.__at_put(empty_idx + 0, k, v)
          self.__at_put(kofs,     RemoteNil, empty_idx )
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
          self.__at_put(empty_idx + 2, nil) # auto-grow, chain terminator, fix 768
          self.__at_put(last_idx + 2, empty_idx)
        end
        self.__at_put(empty_idx, key, value)
      else
        raise 'Inconsistent Hash collision bucket'
        return 0
      end
      @_st_numElements = @_st_numElements + 1
      # @_st_numCollisions  not maintained in buckets
    else
      # empty entry in hash table
      self.__at_put(kofs , key, value)
      @_st_numElements = @_st_numElements + 1
    end
    return 1
  end

  def __delete(key) # [
    # returns value removed, or RemoteNil
    kh = key.hash
    kofs = kh % @_st_tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          self.__at_put(kofs, RemoteNil, RemoteNil)
          @_st_numElements = @_st_numElements - 1
          return v
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          ck = self.__at(idx)
          if key.eql?( ck )
            v = self.__at(idx + 1)
            self.__at_put(idx , RemoteNil, RemoteNil)
            @_st_numElements = @_st_numElements - 1
            @_st_numCollisions = @_st_numCollisions - 1
            return v
          end
          idx = self.__at(idx + 2)
        end while idx._isFixnum
      else
        # a collision bucket
        val = v.__bucket_delete( key, kh )
        if val._not_equal?(RemoteNil)
          @_st_numElements = @_st_numElements - 1
          @_st_numCollisions = @_st_numCollisions - 1
        end
      return val
      end
    end
    return RemoteNil # not found
  end # ]

  def __bucket_delete( key, khash )
    kofs = khash % @_st_tableSize  ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key.eql?( k )
          self.__at_put(kofs, RemoteNil, RemoteNil)
          @_st_numElements = @_st_numElements - 1
          return v
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          ck = self.__at(idx)
          if key.eql?( ck )
            v = self.__at(idx + 1)
            self.__at_put(idx , RemoteNil, RemoteNil)
            @_st_numElements = @_st_numElements - 1
            # @_st_numCollisions not maintained in buckets
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

  def delete(key, &block)
    v = self.__delete(key)
    if v._equal?(RemoteNil)
      if block_given?
        return  block.call(key)
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
    ts = @_st_tableSize
    if ts > 20
      ts = 19
    end
    self.__clear(ts)
  end

  primitive '__prime_sizes', '_primeTableSize:'

  def __clear(new_size)
    arr = self.__prime_sizes(new_size)
    @_st_collisionLimit = arr.__at(1)
    ts = arr.__at(0)
    @_st_tableSize = ts
    two_ts = ts + ts
    self.__fill_resize(1, two_ts, RemoteNil) # one-based first arg
    @_st_numElements = 0
    @_st_numCollisions = 0
    self
  end

  def default(key=MaglevUndefined)
    if key._equal?(MaglevUndefined)
      return self.default()
    end
    if @_st_defaultIsBlock
      @_st_defaultOrParent.call(self, key)
    else
      @_st_defaultOrParent
    end
  end

  def default
    if @_st_defaultIsBlock
      @_st_defaultOrParent.call(nil)
    else
      @_st_defaultOrParent
    end
  end

  def __parent
    # message only valid for collision buckets
    @_st_defaultOrParent
  end

  def __parent=(h)
    # message only valid for collision buckets
    @_st_defaultOrParent = h
  end

  def default=(value)
    # returns receiver
    @_st_defaultOrParent = value
    @_st_defaultIsBlock = value._isBlock
    self
  end

  def default_proc
    if @_st_defaultIsBlock
      @_st_defaultOrParent
    else
      nil
    end
  end

  def __store_defaults(value, is_block)
  end

  def delete_if(&block)
    # RUBINIUS: This code is from rubinius core/hash.rb ;  modified.
    # maglev 1.8.7 does not support returning an Enumerator due to complex side effects
    unless block_given?
      if @_st_numElements._equal?(0)
        return nil
      end    
      raise LocalJumpError, "no block given"  # changes for 1.8.7
    else
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
    res = @_st_collisionLimit
    @_st_collisionLimit = cl
    res
  end

  primitive_nobridge '__become', '_becomeMinimalChecks:'
  primitive_nobridge '__basic_dup_named_ivs', '_rubyBasicDupNamedIvs'

  def __rebuild(new_size)
    # puts "start rebuild ( "
    nhash = self.__basic_dup_named_ivs
    nhash.__clear(new_size) 
    nhash.default=( @_st_defaultOrParent )
    save_cl = nhash.__set_collision_limit(Fixnum_MAX) # prevent recursive rebuild

    nhash.__become(self) # before populating new implementation
       #   so new buckets get proper parent ref
    nhash.__merge_into(self)

    self.__set_collision_limit(save_cl)
    # puts " ) end rebuild to ts #{@_st_tableSize} from ts #{nhash.__tableSize} size #{@_st_numElements}"
    self
  end

  def rehash
    if self.frozen?
      raise TypeError, 'rehash called on frozen instance of Hash'
    end
    self.__rebuild(@_st_numElements)
  end

  def __merge_into(other)
    lim = @_st_tableSize
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
    unless block_given?
      return HashEnumerator.new(self, :each_pair) # for 1.8.7
    end
    num_elem = @_st_numElements
    lim = @_st_tableSize
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
          # a collision bucket , which is a small Hash 
          v.each_pair(&block)
        end
      end
      kofs += 2
    end
    # Use >, not .equal?, since MRI allows deletes, but not adds
    if @_st_numElements > num_elem
      raise RuntimeError, 'Hash changed during iteration'
    end
    self
  end

  alias each each_pair

  def each_key(&block)
    unless block_given?
      return HashKeyEnumerator.new(self, :each_key) # for 1.8.7
    end
    lim = @_st_tableSize
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
            # a collision bucket , which is a small Hash 
            v.each_key(&block)
          end
        end
      end
      kofs += 2
    end
    self
  end

  def each_value(&block)
    unless block_given?
      return HashKeyEnumerator.new(self, :each_value) # for 1.8.7
    end
    lim = @_st_tableSize
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
          # a collision bucket , which is a small Hash 
          v.each_value(&block)
        end
      end
      kofs += 2
    end
    self
  end

  def empty?
    @_st_numElements._equal?(0)
  end

  # Return a value from the hash for the given +key+.  If +key+ is not
  # found, one of the following is returned:
  # * If no other parameters passed, Raise +IndexError+.
  # * If +default+ is given, return it.
  # * If +block+ is given, return the value of calling +block+ with +key+
  # Fetch does not use any default values supplied when the hash was created.
  #

  def fetch(key, dflt=MaglevUndefined, &block)
    udef = MaglevUndefined
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
    result = Hash.__new(@_st_tableSize)
    self.each_pair { |k,v|
      result.__atkey_put( v,  k )
    }
    result
  end

  def keys
    arr = Array.new(@_st_numElements)
    idx = 0
    self.each_key { | k |
      arr.__at_put(idx, k)
      idx += 1
    }
    arr
  end

  alias key? has_key?

  def length
    @_st_numElements
  end

  def merge(other, &block)
    other = Maglev::Type.coerce_to(other, Hash, :to_hash)
    other_siz = other.size
    my_siz = @_st_numElements
    res_siz = ((my_siz + other_siz).to_f * 1.4 ).to_i
    if res_siz > (ts = @_st_tableSize)  && ts < 1009
      h = self.class.__new(res_siz)
      h.default=( @_st_defaultOrParent )
      self.__merge_into(h)
    else
      h = self.dup
    end
    h.__merge!(other, &block)
  end

  def merge!(other, &block)
    other = Maglev::Type.coerce_to(other, Hash, :to_hash)
    other_siz = other.size
    my_siz = @_st_numElements
    if other._not_equal?(self)
      res_siz = ((my_siz + other_siz).to_f * 1.4 ).to_i
      if res_siz > (ts = @_st_tableSize)  && ts < 1009
        self.__rebuild(res_siz)
      end
    end
    self.__merge!(other, &block)
  end

  def __merge!(other, &block)
    if block_given?
      if other._equal?(self)
        pairs_siz = @_st_numElements  * 2
        pairs = Array.new( pairs_siz )
        n = 0
        self.each_pair { |k, v|
          pairs[n] = k
          pairs[n+1] = v
          n += 2
        }
        n = 0
        while n < pairs_siz
          k = pairs.__at(n)
          v = pairs.__at(n + 1)
          self.__atkey_put(k,  block.call(k, v, v))
          n += 2
        end
      else
        other.each_pair { |k, v|
          if self.has_key?( k)
            self.__atkey_put(k,  block.call(k, self.__atkey(k), v))
          else
            self.__atkey_put(k, v)
          end
        }
      end
    elsif other._not_equal?(self)
      other.__merge_into(self)
    end
    self
  end

  def reject(&block)
    unless block_given?
      return HashEnumerator.new(self, :reject ) # for 1.8.7
    end
    self.dup.delete_if(&block)
  end

  def reject!(&block)
    old_size = size
    delete_if(&block)
    return nil if old_size == size
    self
  end

  # def reject!() ; end # 1.8.7 , not implemented yet due to 
                        #  complex side effects of   Enumerator#next

  def replace(hash)
    hash = Maglev::Type.coerce_to(hash, Hash, :to_hash)
    self.__clear( hash.size )
    hash.__merge_into(self)
    self
  end

  def select(&block)
    unless block_given?
      return HashEnumerator.new(self, :select) # for 1.8.7
    end
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
    arr = Array.new(@_st_numElements)
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
    lim = @_st_tableSize
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
    return "{}" if @_st_numElements._equal?(0)
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
