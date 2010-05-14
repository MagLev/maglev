module OpenSSL
  class HMACError < OpenSSLError; end

  # Wraps the OpenSSL Hashed Message Authentication Code (HMAC)
  # abstraction.
  class HMAC

    # call-seq:
    #   HMAC.digest(digest, key, data) -> aString
    #
    # Returns a string that is the MAC for the given data, key and digest
    # algorithm.  The digest can be either a string (e.g., "SHA1") or one
    # of the OpenSSL::Digest subclasses. There are several ways to get a
    # digest:
    #    <tt>digest = OpenSSL::Digest.new('SHA1')</tt>
    #    <tt>OpenSSL::Digest::SHA1.new</tt>
    def self.digest(digest, key, data)
      key_v  = Maglev::RubyUtils.rb_string_value(key)
      data_v = Maglev::RubyUtils.rb_string_value(data)
      d = Digest.get_digest_ptr(digest)

      result = FFI::MemoryPointer.new(:string, OpenSSL::Digest::EVP_MAX_MD_SIZE)
      result_len = FFI::MemoryPointer.new(:uint)
      OpenSSL::LibCrypto.HMAC(d, key_v, key_v.length,
                              data_v, data_v.length,
                              result, result_len)
      result.read_string
    end

    # call-seq:
    #   HMAC.hexdigest(d, key, data) -> aString
    #
    # Calls HMAC.digest and then converts to hex.
    def self.hexdigest(d, key, data)
      self.digest(d, key, data).unpack('H*')[0]
    end

    def initialize
      raise NotImplementedError
    end

    # copy

    def reset
      raise NotImplementedError
    end

    def update
      raise NotImplementedError
    end

    def <<
      raise NotImplementedError
    end

    def digest
      raise NotImplementedError
    end

    def hexdigest
      raise NotImplementedError
    end

    alias :inspect :hexdigest
    alias :to_s :hexdigest
  end
end
