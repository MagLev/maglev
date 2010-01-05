class Env
    # A ruby reference to ENV that resolves to ::ENV results in access
    # to a  transient Ruby constant , whose value is
    # the one transient instance of Env , stored in  SessionTemps current .

    class_primitive_nobridge '__getenv', '_getenv:'
    class_primitive_nobridge '__putenv', '_putenv:with:'

    def self.new
      raise NotImplementedError
    end

    def self.[](*elements)
      raise NotImplementedError
    end

    def dup
      res = Hash.new
      res.update(self)
      res
    end

    def clone
      self.dup
    end

    def [](key)
      v = __atkey(key)
      if v._equal?(nil)
        v = Env.__getenv(key)
        unless v._equal?(nil)
          self.__atkey_put( key, v ) # __atkey_put implemented in Hash
        end
      end
      v
    end

    def []=(key, val)
      if key._isString
        if key.index('GEMSTONE')._equal?(0)
          raise 'you may not change GEMSTONE* environment variables from within maglev'
        end
        if key.index('MAGLEV')._equal?(0) and key != 'MAGLEV_OPTS'
          raise 'you may not change MAGLEV* environment variables from within maglev'
        end
      end
      if val._equal?(nil)
        val = ""
      end
      Env.__putenv(key, val)
      self.__atkey_put(key, val) # __atkey_put implemented in Hash 
    end

    def store(key, val)
     self.[]=(key, val)
    end

    def clear
      raise NotImplementedError
    end

    def delete(aKey, &aBlock)
      # can't delete from C environment, can only []=(aKey,'')
      raise NotImplementedError
    end
    def delete(aKey)
      raise NotImplementedError
    end
    def delete_if(&block)
       raise NotImplementedError
    end
    def merge(aHash)
      raise NotImplementedError
    end
    def merge!(other)
      raise NotImplementedError
    end

    def to_s
      "ENV"
    end
end

