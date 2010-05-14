module OpenSSL

  # FFI wrapper for the OpenSSL libcrypto library.  This provides access to
  # Message Digests (SHA1), etc.
  #
  # The library is initialized when this file is loaded.
  class LibCrypto
    extend FFI::Library

    # TODO: dev builds are messed up and add .dylib.dylib on end
    ffi_lib "#{ENV['GEMSTONE']}/lib/libcrypto"

    # Wraps the <tt>EVP_MD_CTX</tt> struct that holds the message digest
    # information for a digest object.
    #
    #--
    # struct env_md_ctx_st
    #   {
    #   const EVP_MD *digest;
    #   ENGINE *engine; /* functional reference if 'digest' is ENGINE-provided */
    #   unsigned long flags;
    #   void *md_data;
    #   } /* EVP_MD_CTX */;
    #++
    class MDContext < FFI::Struct
      layout(:digest, :pointer,
             :engine, :pointer,
             :flags,  :ulong,
             :data,   :pointer)
      # Return the message digest pointer.  Equivalent to
      # <tt>EVP_MD_CTX_md(ctx)</tt>.
      def md
        self[:digest]
      end
    end

    #--
    # void OpenSSL_add_all_digests(void)
    #++
    attach_function(:OpenSSL_add_all_digests, [], :void)

    #--
    # const EVP_MD *EVP_get_digestbyname(const char *name)
    #++
    attach_function(:EVP_get_digestbyname, [:string], :pointer)

    #--
    # int EVP_DigestInit_ex(EVP_MD_CTX *ctx, const EVP_MD *type, ENGINE *impl)
    #++
    attach_function(:EVP_DigestInit_ex, [:pointer, :pointer, :pointer], :int)

    #--
    # EVP_MD_CTX *EVP_MD_CTX_create(void)
    #++
    attach_function(:EVP_MD_CTX_create, [], MDContext)

    #--
    # void EVP_MD_CTX_destroy(EVP_MD_CTX *ctx)
    #++
    attach_function(:EVP_MD_CTX_destroy, [:pointer], :void)

    # HMAC() computes the message authentication code of the n bytes at d
    # using the hash function evp_md and the key key which is key_len bytes
    # long.
    #--
    # unsigned char *HMAC(const EVP_MD *evp_md,
    #                     const void *key,
    #                     int key_len,
    #                     const unsigned char *d,
    #                     size_t n,
    #                     unsigned char *md,
    #                     unsigned int *md_len)
    #++
    attach_function(:HMAC,
                    [:pointer, :const_string, :int, :const_string, :size_t,
                     :pointer, :pointer],
                    :string)

    # Initialize libcrypto.  This performs the following, one-time
    # initialization calls:
    # + OpenSSL_add_all_digests    # Make SHA1 etc. available
    def self.initialize_library
      unless @initialized
        @initialized = true
        self.OpenSSL_add_all_digests
      end
    end
  end

  LibCrypto.initialize_library
end
