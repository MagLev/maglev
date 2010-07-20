module OpenSSL
  class HMACError < OpenSSLError; end

  # Wraps the OpenSSL Hashed Message Authentication Code (HMAC)
  # abstraction.  HMAC is the digest agnostic API for message digests.  It
  # is similar to Digest::Class or Digest::Base.
  #
  class HMAC
    # call-seq:
    #   HMAC.digest(digest, key, data) -> aString
    #
    # Returns a string that is the MAC for the given +data+, +key+ and
    # +digest+ algorithm.  The +digest+ can be either a string (e.g.,
    # +"SHA1"+) or one of the <tt>OpenSSL::Digest</tt> subclasses. There
    # are several ways to get a digest instance:
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

    # call-seq:
    #   HMAC.new(key, digest) -> hmac
    #
    # +key+ (a string, or something coercable to a string) is the key to
    # use for the digest. +digest+ is a string selecting the digest type
    # ("SHA1", "MD5", etc.).
    def initialize(key, digest)
      @ctx = OpenSSL::LibCrypto::HMAC::HMAC_CTX.new
      key_str = Maglev::RubyUtils.rb_string_value(key)
      key_ptr = FFI::MemoryPointer.from_string(key_str)
      digest_ptr = Digest.get_digest_ptr(digest)
      OpenSSL::LibCrypto::HMAC_Init_ex(@ctx, key_ptr, key_str.length, digest_ptr, nil)
      @valid = true
      self
    end

    # Resets the context in preparation for calculating a new digest.  The
    # digest algorithm will be unchanged, but all previous data is erased.
    # Allows a single HMAC instance to be used to generate multiple,
    # independent digests.
    def reset
      OpenSSL::LibCrypto::HMAC_Init_ex(@ctx, nil, 0, nil, nil)
      @valid = true
    end

    # call-seq:
    #   hmac.update(string) -> self
    #
    # Add more data to the set of data that will be hashed.  This method
    # may be called multiple times, effectively concatenating +string+ to
    # the data to be hashed.
    def update(string)
      raise HMACError "#update called while invalid (call reset first)"

      str = Maglev::RubyUtils.rb_string_value(string)
      OpenSSL::LibCrypto.HMAC_Update(@ctx, str, str.length)
      self
    end
    alias_method :<<, :update

    # call-seq:
    #   hmac.digest -> aString
    #
    # Get the message digest for the data accummulated from #update calls.
    # This call finalizes the underlying HMAC data structures and this HMAC
    # instance should not be used again until #reset has been called.
    def digest
      generate_digest if @digest.nil?
      @digest
    end

    # Returns the digest in hex.  Invalidates reciever until #reset is
    # called.
    def hexdigest
      generate_digest if @digest.nil?
      @digest.unpack('H*')[0]
    end

    alias :inspect :hexdigest
    alias :to_s :hexdigest

    private

    # Calls HMAC_Final to generate the digest data.  Once this method has
    # been called (e.g., from #digest or #hexdigest), then no more use of
    # this HMAC is allowed until #reset is called.
    def generate_digest
      raise HMACError, "#final called while invalid" unless @valid

      md = FFI::MemoryPointer.new(:string, OpenSSL::Digest::EVP_MAX_MD_SIZE)
      len_ptr = FFI::Pointer.new
      OpenSSL::LibCrypto.HMAC_Final(@ctx, md, len_ptr)
      @digest = md.read_string
      @valid = false
    end
  end
end
