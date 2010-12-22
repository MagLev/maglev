module OpenSSL
  module Random
    class RandomError < OpenSSLError; end

    def seed(string)
      OpenSSL::LibCrypto.RAND_seed(string, string.length)
      string
    end
    module_function :seed

    # call-seq:
    #   add(string, entropy)
    def random_add(string, entropy)
      OpenSSL::LibCrypto.RAND_add(string, string.length, entropy)
      self
    end
    module_function :random_add

    # call-seq:
    #   random_bytes(length) -> aString
    def random_bytes(length)
      string_ptr = FFI::MemoryPointer.new(:char, length)
      if OpenSSL::LibCrypto.RAND_bytes(string_ptr, length) != 1
        raise RandomError.new("Problem generating #{length} random bytes (#{result})")
      end
      string_ptr.read_string(length)
    end
    module_function :random_bytes

    # call-seq:
    #   pseudo_bytes(length) -> aString
    def pseudo_bytes(length)
      string_ptr = FFI::MemoryPointer.new(:char, length)
      if OpenSSL::LibCrypto.RAND_pseudo_bytes(string_ptr, length) != 1
        raise RandomError.new("Problem generating #{length} pseudo bytes (#{result})")
      end
      string_ptr.read_string(length)
    end
    module_function :pseudo_bytes

    # DEFMETH(mRandom, "load_random_file", ossl_rand_load_file, 1);
    # DEFMETH(mRandom, "write_random_file", ossl_rand_write_file, 1);
    # DEFMETH(mRandom, "egd", ossl_rand_egd, 1);
    # DEFMETH(mRandom, "egd_bytes", ossl_rand_egd_bytes, 2);
    # DEFMETH(mRandom, "status?", ossl_rand_status, 0)
  end
end
