require 'digest'

module OpenSSL

  # This class should wrap a struct EVP_MD_CTX_create()
  # and call EVP_MD_CTX_destroy() on the context
  class Digest < Digest::Class

    SHA1 = :SHA1

    class DigestError < OpenSSLError; end

    # call-seq:
    #   Digest.new(string) -> digest
    def initialize(string)
      @ctx = allocate_finalized_ctx
      md = get_digest(
      OpenSSL::LibCrypto.EVP_DigestInit_ex(@ctx, md, nil)
    end

    def reset
    end

    def update
    end

    def <<
    end

    def finish
    end

    def digest_length
    end

    def block_length
    end

    private

    # Allocate and return an EVP_MD_CTX context.  Registers a finalizer to
    # destroy the context.  Raises a RuntimeError if can't allocate the
    # context.
    def self.allocate_finalized_ctx
      ctx = OpenSSL::LibCrypto.EVP_MD_CTX_create
      # TODO: what is null pointer check?
      raise 'EVP_MD_CTX_create() failed' unless ctx

      finalizer = Proc.new do |o|
        OpenSSL::LibCrypto.EVP_MD_CTX_destroy(o)
      end
      ObjectSpace.define_finalizer(ctx, finalizer)
      ctx
    end

    def get_digest_ptr(type)
      if type._isString?
        name = type # TODO: intern type to get name STR2CSTR(type)
        md = EVP_get_digestbyname(type)
        raise "Unsupported digest algorithm (#{name})." if md.nil?
      else
        md = EVP_MD_CTX_md(@ctx)
      end
      md
    end
    
  end
end
