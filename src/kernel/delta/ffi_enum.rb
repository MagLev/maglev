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

    def _kv_pairs
      res = []
      @kv_map.each_pair { | k , v |
        res << k
        res << v
      }
      res
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

    def _sym_to_value(symbol)
      # called from a method derived from a CFunction template
      @kv_map[symbol]
    end

    def _val_to_symbol(a_fixnum)
      # called from a method derived from a CFunction template
      @vk_map[a_fixnum]
    end

  end

#   class Enums
#     # def initialize ; end #  in ffi.rb

#     ALL_Enums = []    # TODO : persistent/transient copies of these 3 constants
#     ALL_NamedEnums = IdentityHash.new
#     ALL_kv_map = IdentityHash.new

#     # TODO, ALL_kv_map needs to be accessed via C from om::fetchCFunctionArg
#     #  in capiprim.c to coerce Symbols to CFunc_Arg_I8..CFunc_Arg_I64 values
#     #   and keyvaluedict.c      needs new support for IdentityKeyValueDictionary

#     def <<(enum)
#       # Maglev disallows duplicate names of Enums and duplicate occurance of symbols
#       #  within Enums.
#       kv_map = ALL_kv_map
#       pairs = enum._kv_pairs
#       (0..pairs.size-1).step(2) { |n|
#         sym = pairs[n]
#         if kv_map.has_key?(sym)
#           prev_enum = self._enum_for_symbol(sym)
#           raise ArgumentError, "name #{sym} in Enum #{enum._printable_name} previously defined in Enum #{prev_enum._printable_name"
#         end
#       }
#       nam = enum.name
#       if nam._isSymbol
#         all_enums = ALL_NamedEnums
#         if all_enums.has_key?(nam)
#           raise ArgumentError, "Enum #{nam} has already been defined"
#         end
#         all_enums[nam] = enum
#       end
#       ALL_Enums << enum
#       all_kvs = ALL_kv_map
#       (0..pairs.size-1).step(2) { |n|
#         all_kvs[ pairs[n] ] = pairs[n+1]
#       }
#     end

#     def _enum_for_symbol(sym)
#       # returns an Enum which has sym as a named value, or nil
#       if s._isSymbol
#         ALL_Enums.each {|e|
#           if e._sym_to_value(sym)
#             return e
#           end
#         }
#       end
#       nil
#     end

#     def find(query)
#       e = ALL_NamedEnums[query]
#       if e._not_equal?(nil)
#         return e  # an Enum with name == query
#       end
#       return enum_for_symbol(query) # an unnamed Enum containing query as a symbol
#     end

#     def __map_symbol(symbol)
#       ALL_kv_map[symbol]
#     end

#   end

end
