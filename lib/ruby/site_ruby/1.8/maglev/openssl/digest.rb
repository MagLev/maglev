require 'digest'

module OpenSSL

  # This class should wrap a struct EVP_MD_CTX_create()
  # and call EVP_MD_CTX_destroy() on the context
  class Digest < Digest::Class

    EVP_MAX_MD_SIZE = 64      # From openssl evp.h

    class DigestError < OpenSSLError; end

    # call-seq:
    #   Digest.new(digest_name) -> digest
    #
    # Recognized digests include: 'SHA1'
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
    # Rest the digest.  Preserves the message digest type.
    def reset
      md = @ctx.md
      OpenSSL::LibCrypto.EVP_DigestInit_ex(@ctx, md, nil)
      self
    end

    # call-seq:
    #  digest.update(string) -> self
    def update
      string_v = Maglev::RubyUtils.rb_string_value(string)
      OpenSSL::LibCrypto.EVP_DigestUpdate(@ctx, string_v, string_v.len)
      self
    end

    def <<(other)
      raise NotImplementedError
    end

    def finish
      raise NotImplementedError
    end

    def digest_length
      raise NotImplementedError
    end

    def block_length
      raise NotImplementedError
    end

    def name
      @name
    end

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

    private

    # Allocate and return an EVP_MD_CTX context.  Registers a finalizer to
    # destroy the context.  Raises a RuntimeError if can't allocate the
    # context.
    def self.ossl_digest_alloc
      cptr = OpenSSL::LibCrypto.EVP_MD_CTX_create
      raise 'EVP_MD_CTX_create() failed' if cptr.null?
      ctx = OpenSSL::LibCrypto::MDContext.new(cptr)

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

    class SHA1 < Digest
      def initialize(*data)
        super('SHA1', data.first)
      end

      def self.digest(data)
        Digest.digest('SHA1', data)
      end

      def self.hexdigest(data)
        Digest.hexdigest('SHA1', data)
      end
    end

    # %w(SHA1 MD5).each do |name|
    #   klass = Class.new(Digest) do
    #     define_method(:initialize) do |*data|
    #       if data.length > 1
    #         raise ArgumentError,
    #         "wrong number of arguments (#{data.length} for 1)"
    #       end
    #       super(name, data.first)
    #     end
    #   end

    #   singleton = class << klass; self; end
    #   singleton.class_eval {
    #     define_method(:digest)    { |data| Digest.digest(name, data)    }
    #     define_method(:hexdigest) { |data| Digest.hexdigest(name, data) }
    #   }
    #const_set(name, klass)
    # end
  end
end


