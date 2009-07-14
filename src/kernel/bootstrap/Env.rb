class Env
    # A ruby reference to ENV that resolves to ::ENV results in access
    #  to a  RubyTransientConstantAssociation , which redirects access to 
    # the one transient instance of Env , stored in  SessionTemps current .

    class_primitive_nobridge '_getenv', '_getenv:'
    class_primitive_nobridge '_putenv', '_putenv:with:'

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

    primitive '_at_put' , 'rubyAt:put:' # resolves to RubyHash>>rubyAt:put:

    def [](key)
      v = super(key)
      if v.equal?(nil)
        v = Env._getenv(key)
        unless v.equal?(nil)
          self._at_put(key, v)
        end
      end
      v
    end

    def []=(key, val)
      Env._putenv(key, val)
      super(key, val)
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

end

