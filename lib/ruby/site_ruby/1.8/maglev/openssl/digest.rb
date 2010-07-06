require 'digest'  # lib/ruby/1.8/digest.rb

# TODO: Can we rip out the EVP layer and only use HMAC instead (via hmac.rb)?

module OpenSSL

  #--
  # This class should wrap a struct EVP_MD_CTX_create()
  # and call EVP_MD_CTX_destroy() on the context
  #
  # See the documentation under lib/ruby/1.8/digest.rb for details on the
  # Digest API.
  #++
  #
  # This class is a generic implementation of ::Digest using the HMAC
  # abstraction to support all message digests provided by OpenSSL.
  class Digest < Digest::Class

    EVP_MAX_MD_SIZE = 64      # From openssl evp.h

    class DigestError < OpenSSLError; end

    # Array of supported message digests algorithms
    SUPPORTED_DIGESTS =
      ["DSS1", "MD2", "MD4", "MD5", "RIPEMD160", "SHA", "SHA1",
       "SHA224", "SHA256", "SHA384", "SHA512" ]

    def self.digest(name, data)
        super(data, name)
    end

    # call-seq:
    #   Digest.new(digest_name, data=nil) -> digest
    #
    # Initializes a new digest implementation.  +digest_name+ is one of the
    # message digest algorithm to use.  If +data+ is not nil, then a call
    # to #update is made, passing +data+.
    #
    # Recognized digests include (see SUPPORTED_DIGESTS):
    #   "DSS1" "MD2" "MD4" "MD5" "RIPEMD160" "SHA" "SHA1"
    #   "SHA224" "SHA256" "SHA384" "SHA512"
    #
    def initialize(digest_name, data=nil)
      @name = digest_name
      @ctx = Digest.ossl_digest_alloc
      message_digest = OpenSSL::LibCrypto.EVP_get_digestbyname(digest_name)
      OpenSSL::LibCrypto.EVP_DigestInit_ex(@ctx, message_digest, nil)
      self.update(data) unless data.nil?
    end

    # call-seq:
    #   digest.reset -> self
    #
    # Reset this digest object to accept new data.  Preserves the message
    # digest type.  All accummulated data from previous calls to #update is
    # erased.
    def reset
      md = @ctx.md
      OpenSSL::LibCrypto.EVP_DigestInit_ex(@ctx, md, nil)
      self
    end

    # call-seq:
    #  digest.update(string) -> self
    #
    # Adds more data to be digested by this digest.  This method can be
    # called multiple times to accumulate the data.  Returns self.
    def update(string)
      string_v = Maglev::RubyUtils.rb_string_value(string)
      string_ptr = FFI::MemoryPointer.from_string(string_v)
      OpenSSL::LibCrypto.EVP_DigestUpdate(@ctx, string_ptr, string_v.length)
      self
    end
    alias_method :<<, :update

    # Returns the length of the digest string, given the current data for
    # this digest object.  Some digests (MD5) always return the same length
    # digest strings, others return digests of varying length.
    def digest_length
      @ctx.digest_length
    end

    def block_length
      raise NotImplementedError, "#{self.class.name}#block_name"
    end

    # Returns the name of the digest algorithm.
    def name
      @name
    end

    # Return the message digest object.
    def md
      @ctx.md
    end

    # A +Proc+ that does finalization on message digest contexts.
    #
    # Note: MagLev finalizers get passed the object to be finalized, not
    # the object id.
    FINALIZER = Proc.new do |o|
      OpenSSL::LibCrypto.EVP_MD_CTX_destroy(o)
    end


    # call-seq:
    #   digest.finish -> digest_string
    #
    # This method produces the digest for all accumulated data since this
    # digest was initialized or last reset.
    def finish
      string_ptr = FFI::MemoryPointer.new(:char, self.digest_length)
      OpenSSL::LibCrypto.EVP_DigestFinal_ex(@ctx, string_ptr)
      string_ptr.read_string
    end

    private

    # Allocate and return an EVP_MD_CTX context.  Registers a finalizer to
    # destroy the context.  Raises a RuntimeError if can't allocate the
    # context.
    def self.ossl_digest_alloc
      cptr = OpenSSL::LibCrypto.EVP_MD_CTX_create
      raise 'EVP_MD_CTX_create() failed' if cptr.null?
      ctx = OpenSSL::LibCrypto::EVP_MD_CTX.new(cptr)

      ObjectSpace.define_finalizer(ctx, FINALIZER)
      ctx
    end

    # Get a digest given a string or a context.
    # E.g., Digest.get_digest_ptr('SHA1'),
    # or Digest.get_digest_ptr(@ctx)
    #--
    # See ossl_digest.c GetDigestPtr
    #++
    def self.get_digest_ptr(type)
      if type.kind_of?(String)
        md = OpenSSL::LibCrypto.EVP_get_digestbyname(type)
        raise "Unsupported digest algorithm (#{name})." if md.nil?
        md
      else
        type.md
      end
    end

    #--
    # The following code creates a sublcass of Digest for each
    # implementation of message digest algorithm supported by openssl.  The
    # class is registered as a constant in Digest (e.g., Digest::SHA1 will
    # be a class that implements the SHA1 message digest algorithm via HMAC
    # and the instance methods defined in this class).
    #++
    SUPPORTED_DIGESTS.each do |name|
      klass = Class.new(Digest){
        define_method(:initialize){|*data|
          if data.length > 1
            raise ArgumentError,
              "wrong number of arguments (#{data.length} for 1)"
          end
          super(name, data.first)
        }
      }
      singleton = (class <<klass; self; end)
      singleton.class_eval{
        define_method(:digest){|data| Digest.digest(name, data) }
        define_method(:hexdigest){|data| Digest.hexdigest(name, data) }
      }
      const_set(name, klass)
    end
  end
end
