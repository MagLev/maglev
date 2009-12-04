module FFI

  class Enum
    # class and simple accessors defined in ffi.rb

    def initialize(arr, name)
      @name = name
      n = 0
      kv_h = IdentityHash.new
      vk_h = IdentityHash.new
      lim = arr.size
      prev_val = -1
      while n < lim
        k = arr[n]
        if k._isSymbol
          if (m = n + 1) < lim
            val = arr[m]
            if val._isFixnum
              kv_h[k] = val
              vk_h[val] = k
              prev_val = val
              n += 2
              next
            end
          end
          val = prev_val + 1
          kv_h[k] = val
          vk_h[val] = k
          prev_val = val
          n += 1
        else
          raise TypeError, 'invalid element of Enum values'
        end
      end
      kv_h.freeze
      @kv_map = kv_h
      vk_h.freeze
      @vk_map = vk_h
    end

    def symbols
      @kv_map.keys
    end

    def __kv_pairs
      res = []
      @kv_map.each_pair { | k , v |
        res << k
        res << v
      }
      res
    end

    def ==(other)
      unless other._kind_of?(Enum)
        return false
      end
      @name._equal?(other.name) && __kv_pairs == other.__kv_pairs
    end

    def hash
      @name.hash | (@kv_map.size << 5)
    end

    def [](query)
      if query._isSymbol
        @kv_map[query]
      elsif query._isFixnum
        @vk_map[query]
      else
        nil
      end
    end
    alias find []

    def __sym_to_value(symbol)
      # called from a method derived from a CFunction template
      @kv_map[symbol]
    end

    def __val_to_symbol(a_fixnum)
      # called from a method derived from a CFunction template
      @vk_map[a_fixnum]
    end

    def enum_value(symbol)
      unless symbol._isSymbol
        raise TypeError , 'expected a Symbol'
      end
      self.__sym_to_value(symbol)
    end
  end

    class Enums
      # def initialize ; end #  in ffi.rb

      def self.__Transient_Enums
        if defined?($__FFI_Enums_TransientEnums)
          arr = $__FFI_Enums_TransientEnums
        else
          arr = []
          $__FFI_Enums_TransientEnums = arr
        end
        arr
      end

      def self.__Transient_NamedEnums
        if defined?($__FFI_Enums_TransientNamedEnums)
          ht = $__FFI_Enums_TransientNamedEnums
        else
          ht = IdentityHash.new
          $__FFI_Enums_TransientNamedEnums = ht
        end
        ht
      end

      def self.__Transient_kv_map
        if defined?($__FFI_Enums_Transient_kv_map)
          ht = $__FFI_Enums_Transient_kv_map
        else
          ht = IdentityHash.new
          $__FFI_Enums_Transient_kv_map = ht
        end
        ht
      end

      def self.__find_named_enum(sym) 
        v = self.__Transient_NamedEnums[sym]
        if v._equal?(nil)
          v = Persistent_NamedEnums[sym]
        end
        v
      end

      def self.__enum_symbol_to_value(sym)
        v = self.__Transient_kv_map[sym]
        if v._equal?(nil)
          v = Persistent_kv_map[sym]
        end
        v
      end

      def self.__add_named_enum(sym, enum)
        # if FFI::DEBUG > 1
        #   puts "added named enum  #{sym}"
        # end
        self.__Transient_NamedEnums[sym] = enum
        if RubyContext.persistence_mode
          Persistent_NamedEnums[sym] = enum
        end
      end

      def self.__add_enum(enum)
        self.__Transient_Enums << enum
        if RubyContext.persistence_mode
           Persistent_Enums << enum
        end
      end

      def self.__find_unnamed_enum(enum) 
        self.__Transient_Enums.each { |e| 
          if e == enum ; return e; end 
        }     
        Persistent_Enums.each { |e| 
          if e == enum ; return e; end 
        }     
        nil
      end

      def self.__add_kv_pairs( pairs )
        tr_map = self.__Transient_kv_map
        pm = RubyContext.persistence_mode
        (0..pairs.size-1).step(2) { |n|
          key = pairs[n]
          val = pairs[n+1]
          #if FFI::DEBUG > 1
          #  puts "added kv pair #{key} --> #{val}"
          #end
          tr_map[ key ] = val
          if pm
            Persistent_kv_map[key] = val
          end
        }
      end

      def self.__enum_for_symbol(sym)
        # returns an Enum which has sym as a named value, or nil
        if sym._isSymbol
          self.__Transient_Enums.each { |e|
            if e.__sym_to_value(sym) ; return e ; end
          }
          Persistent_Enums.each { |e|
            if e.__sym_to_value(sym) ; return e ; end
          }
        end 
        nil
      end

      # TODO, ALL_kv_map needs to be accessed via C from om::fetchCFunctionArg
      #  in capiprim.c to coerce Symbols to CFunc_Arg_I8..CFunc_Arg_I64 values
      #   and keyvaluedict.c      needs new support for IdentityKeyValueDictionary

      def self.add(enum)
        # add the specified enum .
        # Maglev disallows duplicate names of Enums 
        #  and disallows duplicate occurance of symbols within Enums.
        pairs = enum.__kv_pairs
        (0..pairs.size-1).step(2) { |n|
          # check all names in the enum to detect duplicate names wrt previous Enums
          sym = pairs[n]
          val = pairs[n+1]
          if val._equal?(nil)
            raise ArgumentError, "in an Enum, value for #{sym} is nil "
          end
          prev = self.__enum_symbol_to_value(sym) # search kv_maps
          if prev._not_equal?(nil) && prev._not_equal?(val)
            prev_enum = self.__enum_for_symbol(sym)
            raise ArgumentError, "name #{sym} , value #{val} in Enum #{enum.__printable_name} previously defined in Enum #{prev_enum.__printable_name} with different value #{prev} "
          end
        }
        nam = enum.name
        if nam._isSymbol
          if self.__find_named_enum(nam)
            raise ArgumentError, "Enum #{nam} has already been defined"
          end
          self.__add_named_enum(nam, enum)
        end
        self.__add_enum(enum)
        self.__add_kv_pairs( pairs )
      end

      def self.find(query)
        if query._isSymbol
          e = self.__find_named_enum(query)
          if e._not_equal?(nil)
            return e  # an Enum with name == query
          end
          # look for an unnamed Enum containing query as the name of a value 
          return enums_cls.__enum_for_symbol(query) 
        elsif query._kind_of?(Enum)
          nam = query.name
          if nam._not_equal?(nil)
            return self.__find_named_enum(nam)
          end
          return self.__find_unnamed_enum(nam) 
        else
          raise TypeError , 'expected a Symbol or an Enum'
        end
      end
    end
end
