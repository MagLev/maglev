class Env
    def initialize
      #TODO delete @ht after  putenv prim access working
      @ht = { }
    end

    def [](vname)
      # assume using topaz -l
      v = @ht[vname]
      if (v.equal?(nil))
        v = File._environmentAt(vname, false)
        @ht[vname] = v
      end
      v
    end

    def []=(vname,val)
      # TODO, need smalltalk prim to access HostSetEnv
      @ht[vname] = v
    end

    def has_key?(key)
      ! File._environmentAt(key, false).nil?
    end

    alias include? has_key?

    # TODO the rest of the API for Hash needs implementation here
end

ENV = Env.new
