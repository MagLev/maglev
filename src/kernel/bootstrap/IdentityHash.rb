
class IdentityHash

  def self.from_hash(hash)
    siz = hash.size
    h = self.__new(siz)
    hash.each_pair { | k, v |
      h.__atkey_put( k, v )
    }
    h
  end

  def default=(value)
    unless value._equal?(nil)
      raise ArgumentError , 'non-nil default value not supported by IdentityHash'
    end
  end

  # reimplement from Hash those methods that send hash
  #  or do comparison of keys.

  def __atkey(key)  # [
    kh = key.__identity_hash
    kofs = kh % @_st_tableSize  ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key._equal?(k)
          return v
        end
      elsif v._isFixnum  
        idx = v  # internal collision chain
        begin
          if key._equal?(self.__at(idx) )
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
    nil
  end # ]

  alias [] __atkey

  def __bucket_at(key, khash)  
    # parent is a Hash , khash is the result of key.__identity_hash
    # returns value for key, or RemoteNil if key not found
    kofs = khash % @_st_tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key._equal?( k )
          return v
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          if key._equal?( self.__at(idx) )
            return self.__at(idx + 1)
          end
          idx = self.__at(idx + 2)
        end while idx._isFixnum
      end
    end 
    RemoteNil # indicates not-found 
  end

  def __at_orRNil(key)  
    kh = key.__identity_hash
    kofs = kh % @_st_tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key._equal?( k)
          return self.__at(kofs + 1)
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          if key._equal?( self.__at(idx) )
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
      
  def __atkey_put(key, value) # [
    kh = key.__identity_hash
    kofs = kh % @_st_tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key._equal?( k )
          self.__at_put(kofs + 1 , value)
          return value
        else
          # convert entry to collision chain or bucket
          empty_idx = self.__varying_size 
          if empty_idx <= 2014 
            self.__at_put(empty_idx + 4, value, nil ) # auto grows
            self.__at_put(empty_idx + 2, empty_idx + 3, key) 
            self.__at_put(empty_idx + 0, k, v)
            self.__at_put(kofs,     RemoteNil, empty_idx)
          else
            bkt = IdentityHash.__new( 7 )
            bkt.__parent=(self)
            bkt.__bucket_at_put(k, k.__identity_hash , v)
            bkt.__bucket_at_put(key, kh, value)
            self.__at_put(kofs,  RemoteNil, bkt)
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
          elsif key._equal?( ck )
            self.__at_put(idx + 1, value)
            return value 
          end
          last_idx = idx
          idx = self.__at(idx + 2)
        end while idx._isFixnum
        if empty_idx._equal?(nil)   
          # did not find an empty coll chain entry
          empty_idx = self.__varying_size
          self.__at_put(empty_idx + 2, nil) # auto-grows
          self.__at_put(last_idx + 2, empty_idx)
        end  
        self.__at_put(empty_idx, key, value)  
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
        if key._equal?( k )
          self.__at_put(kofs + 1 , value)
          return 0
        else
          # convert entry to collision chain
          empty_idx = self.__varying_size
          self.__at_put(empty_idx + 4, value, nil ) # auto-grows
          self.__at_put(empty_idx + 2, empty_idx + 3, key)
          self.__at_put(empty_idx + 0, k, v)
          self.__at_put(kofs,          RemoteNil, empty_idx )
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
          elsif key._equal?( ck )
            self.__at_put(idx + 1, value)
            return 0 
          end
          last_idx = idx
          idx = self.__at(idx + 2)
        end while idx._isFixnum
        if empty_idx._equal?(nil) 
          # did not find an empty coll chain entry
          empty_idx = self.__varying_size
          self.__at_put(empty_idx + 2, nil) # auto-grows
          self.__at_put(last_idx + 2, empty_idx)
        end  
        self.__at_put(empty_idx, key, value)  
      else
        raise 'Inconsistent Hash collision bucket'
        return 0
      end
    else
      # empty entry in hash table
      self.__at_put(kofs , key, value)
    end
    @_st_numElements = @_st_numElements + 1
    # @_st_numCollisions  not maintained in buckets
    return 1
  end

  def __delete(key) # [  
    # returns value removed, or RemoteNil
    kh = key.__identity_hash
    kofs = kh % @_st_tableSize ; kofs += kofs
    v = self.__at(kofs + 1)
    if v._not_equal?(RemoteNil)
      k = self.__at(kofs)
      if k._not_equal?(RemoteNil)
        if key._equal?( k )
          self.__at_put(kofs , RemoteNil, RemoteNil)
          @_st_numElements = @_st_numElements - 1
          return v
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          ck = self.__at(idx)
          if key._equal?( ck )
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
        if key._equal?( k )
          self.__at_put(kofs  , RemoteNil, RemoteNil)
          @_st_numElements = @_st_numElements - 1
          return v 
        end
      elsif v._isFixnum
        idx = v  # internal collision chain
        begin
          ck = self.__at(idx)
          if key._equal?( ck )
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

  #--
  # Psych/Yaml support.
  # IdentityHash also registered in lib/ruby/1.8/psych.rb
  #++

  # Psych hook method for dumping to YAML
  def encode_with(coder)
    # serialize as a YAML sequence
    coder.represent_map(self.class.name, self)
  end

  # Psych hook method for reviving from YAML
  def init_with(coder)
    coder.map.each_pair { |k,v| self.__atkey_put(k, v) }
  end
end
