module OpenSSL

  # The Random module provides methods return random data.  Random provides
  # several sources of entropy ("randomness"):
  # 1. Random data from a file
  # 2. Random data from entropy provided by the system (e.g., /dev/*random)
  # 3. Random data from the Entropy Generating Daemon (EGD).
  #
  # This implementation does not support the EGD functions: egd(),
  # egd_bytes().  This implelmentation does not support data from files:
  # load_random_file() and write_random_file().
  #
  module Random
    class RandomError < OpenSSLError; end

    # Seed the PRNG state with bytes from +string+.  Equivalent to
    # <tt>random_add(string, string.length)</tt>.
    #
    # @param [String] string random bytes to be mixed into the PRNG state.
    # @return [String] returns the original +string+.
    def seed(string)
      OpenSSL::LibCrypto.RAND_seed(string, string.length)
      string
    end
    module_function :seed

    # Mixes the bytes contained in +string+ into the PRNG state.  If
    # +string+ is data unpredictable to an adversary, then this decreases
    # the predictability of the PRNG output.
    #
    # If they system provides /dev/urandom, then it is used to seed the
    # PRNG state.  If the system does not provide /dev/urandom, then the
    # application must seed the PRNG by calling <tt>random_add</tt>.
    #
    # @param [String] string random bytes to be mixed into the PRNG state.
    # @param [Double] entropy the lower bound of an estimate of how much
    # randomness is contained in +string+, measured in bytes.  For details,
    # see RFC 1750.
    # @return [Object] Returns the receiver of the method call.  This will
    # be OpenSSL::Random, if invoked as a module function.
    def random_add(string, entropy)
      OpenSSL::LibCrypto.RAND_add(string, string.length, entropy)
      self
    end
    module_function :random_add

    # Returns +num+ cryptographically strong random bytes as a String.
    #
    # @param [Fixnum] num The number of random bytes to return.
    # @raise [RandomError] if the PRNG has not been seeded with enough
    # entropy to ensure a cryptographically secure result.
    def random_bytes(num)
      string_ptr = FFI::MemoryPointer.new(:char, num)
      if OpenSSL::LibCrypto.RAND_bytes(string_ptr, num) != 1
        raise RandomError.new("Problem generating #{num} random bytes (#{result})")
      end
      string_ptr.read_string(num)
    end
    module_function :random_bytes

    # Returns +num+ pseudo random bytes as a String.
    #
    # @param [Fixnum] num The number of random bytes to return.
    #
    # @return [String] num random bytes. If +num+ is large enough, the
    # bytes pseudo_rand generates will be unique, but they may be
    # predicatble.  The result may be used for non-cryptographic purposes,
    # and for selected cryptographic purposes (e.g., do NOT use them for
    # key generation).
    #
    # @raise [RandomError] if there is an error.
    def pseudo_bytes(num)
      string_ptr = FFI::MemoryPointer.new(:char, num)
      if OpenSSL::LibCrypto.RAND_pseudo_bytes(string_ptr, num) < 0
        raise RandomError.new("Problem generating #{num} pseudo bytes (#{result})")
      end
      string_ptr.read_string(num)
    end
    module_function :pseudo_bytes

    # Returns true iff the PRNG has been seeded with enough data.
    #
    # @return [Boolean] returns true iff the PRNG has been seeded with
    # enough data.  Returns false otherwise.
    def status?
      OpenSSL::LibCrypto.RAND_status() == 1 ? true : false
    end
    module_function :status?

    def load_random_file(*args)
      raise NotImplementedError, "OpenSSL::Random.load_random_file is not implemented"
    end
    module_function :load_random_file

    def write_random_file(*args)
      raise NotImplementedError, "OpenSSL::Random.write_random_file is not implemented"
    end
    module_function :write_random_file

    def egd(*args)
      raise NotImplementedError, "OpenSSL::Random.egd is not implemented"
    end
    module_function :egd

    def egd_bytes(*args)
      raise NotImplementedError, "OpenSSL::Random.egd_bytes is not implemented"
    end
    module_function :egd_bytes
  end
end
