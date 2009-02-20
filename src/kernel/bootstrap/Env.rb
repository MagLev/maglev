class Env
    # A ruby reference to ENV is translated to the message send Env._current
    class_primitive_nobridge '_current', '_current'
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

    def []=(key, val)
      Env._putenv(key, val)
      super(key, val)
    end

    #  [] implemented in smalltalk because ruby doesn't allow super.[]=(k,v)
    primitive '[]' , 'at:'

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

# access to Global ENV is translated by parser to   Env._current
#  The one instance of Env is stored in   SessionTemps current  
