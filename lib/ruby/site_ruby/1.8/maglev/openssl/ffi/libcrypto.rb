module OpenSSL
  # FFI wrapper for the OpenSSL libcrypto library.  This provides access to
  # Message Digests (SHA1), etc.
  #
  # The library is initialized by calling OpenSSL_add_all_digests when this
  # file is loaded.  This registers all known Message Digest algorithms
  # (SHA1, MD5, etc.).
  class LibCrypto
    extend FFI::Library

    ffi_lib "#{ENV['GEMSTONE']}/lib/libcrypto"

    # Maximum size (in bytes) of a message digest across all implementations
    # supported by libcrypto.
    EVP_MAX_MD_SIZE = 64

    # Wraps the <tt>EVP_MD</tt> struct.
    #--
    #
    # struct env_md_st
    #   {
    #   int type;
    #   int pkey_type;
    #   int md_size;
    #   unsigned long flags;
    #   int (*init)(EVP_MD_CTX *ctx);
    #   int (*update)(EVP_MD_CTX *ctx,const void *data,size_t count);
    #   int (*final)(EVP_MD_CTX *ctx,unsigned char *md);
    #   int (*copy)(EVP_MD_CTX *to,const EVP_MD_CTX *from);
    #   int (*cleanup)(EVP_MD_CTX *ctx);
    #   int (*sign)(int type, const unsigned char *m, unsigned int m_length,
    #         unsigned char *sigret, unsigned int *siglen, void *key);
    #   int (*verify)(int type, const unsigned char *m, unsigned int m_length,
    #           const unsigned char *sigbuf, unsigned int siglen,
    #           void *key);
    #   int required_pkey_type[5]; /*EVP_PKEY_xxx */
    #   int block_size;
    #   int ctx_size; /* how big does the ctx->md_data need to be */
    #   } /* EVP_MD */;
    #++
    class EVP_MD < FFI::Struct
      layout(:type,                :int,
             :pkey_type,           :int,
             :md_size,             :int,
             :flags,               :ulong,
             :init_func,           :pointer,
             :update_func,         :pointer,
             :final_func,          :pointer,
             :copy_func,           :pointer,
             :cleanup_func,        :pointer,
             :sign_func,           :pointer,
             :verify_func,         :pointer,
             :required_pkey_type, [ :char, 5 ],
             :block_size,          :int,
             :ctx_size,            :int)

      # Return the size of the message digest
      def md_size
        self[:md_size]
      end
    end

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
    class EVP_MD_CTX < FFI::Struct
      layout(:digest, :pointer,
             :engine, :pointer,
             :flags,  :ulong,
             :data,   :pointer)
      # Return the message digest pointer.  Equivalent to
      # <tt>EVP_MD_CTX_md(ctx)</tt>.

      def get_digest
        attempt = 1
        case attempt
        when 0
          puts "---- Base code"
          ptr = self[:digest]
          puts "-- EVP_MD_CTX.get_digest: ptr: #{ptr.inspect}"
          EVP_MD.new(ptr)
        when 1
          puts "---- Struct#__fromCPointer:"
          ptr = self[:digest]
          puts "-- EVP_MD_CTX.get_digest: ptr: #{ptr.inspect}"
          layout = EVP_MD.layout
          cptr = FFI::CPointer.__new_from(ptr)
          puts "-- EVP_MD_CTX.get_digest: cptr: #{cptr.inspect}"
          inst = EVP_MD.__fromCPointer(cptr, layout.size)
          inst.__set_layout(layout)
          inst.initialize
          inst
        end
      end
      def digest_length
        get_digest.md_size
      end
      def md
        self[:digest]
      end
    end

    #--
    # void OpenSSL_add_all_digests(void)
    #++
    #
    # Loads all of the digests known to the libopenssl and makes them
    # available.  Must be called before EVP_get_digestbyname etc.
    attach_function(:OpenSSL_add_all_digests, [], :void)

    #--
    # const EVP_MD *EVP_get_digestbyname(const char *name)
    #++
    #
    # EVP_get_digestbyname(), EVP_get_digestbynid() and
    # EVP_get_digestbyobj() return an EVP_MD structure when passed a digest
    # name, a digest NID or an ASN1_OBJECT structure respectively. The
    # digest table must be initialized using, for example,
    # OpenSSL_add_all_digests() for these functions to work.
    attach_function(:EVP_get_digestbyname, [:string], :pointer)

    #--
    # int EVP_DigestInit_ex(EVP_MD_CTX *ctx, const EVP_MD *type, ENGINE *impl)
    #++
    #
    # EVP_DigestInit_ex() sets up digest context ctx to use a digest type
    # from ENGINE impl. ctx must be initialized before calling this
    # function. type will typically be supplied by a functionsuch as
    # EVP_sha1(). If impl is NULL then the default implementation of digest
    # type is used.
    attach_function(:EVP_DigestInit_ex, [:pointer, :pointer, :pointer], :int)

    #--
    # int EVP_DigestUpdate(EVP_MD_CTX *ctx, const void *d, size_t cnt);
    #++
    #
    # EVP_DigestUpdate() hashes cnt bytes of data at d into the digest
    # context ctx. This function can be called several times on the same
    # ctx to hash additional data.
    attach_function(:EVP_DigestUpdate, [:pointer, :pointer, :size_t], :int)

    #--
    # int EVP_DigestFinal_ex(EVP_MD_CTX *ctx, unsigned char *md,
    #                        unsigned int *s);
    #++
    # EVP_DigestFinal_ex() retrieves the digest value from ctx and places
    # it in md. If the s parameter is not NULL then the number of bytes of
    # data written (i.e. the length of the digest) will be written to the
    # integer at s, at most EVP_MAX_MD_SIZE bytes will be written. After
    # calling EVP_DigestFinal_ex() no additional calls to
    # EVP_DigestUpdate() can be made, but EVP_DigestInit_ex() can be called
    # to initialize a new digest operation.
    attach_function(:EVP_DigestFinal_ex, [:pointer, :pointer, :pointer], :int)

    #--
    # EVP_MD_CTX *EVP_MD_CTX_create(void)
    #++
    #
    # EVP_MD_CTX_create() allocates, initializes and returns a digest
    # context.
    attach_function(:EVP_MD_CTX_create, [], EVP_MD_CTX)

    #--
    # void EVP_MD_CTX_destroy(EVP_MD_CTX *ctx)
    #++
    #
    # EVP_MD_CTX_destroy() cleans up digest context ctx and frees up the
    # space allocated to it, it should be called only on a context created
    # using EVP_MD_CTX_create().
    attach_function(:EVP_MD_CTX_destroy, [:pointer], :void)

    #-- ############################################################
    #                           HMAC Layer
    #++ ############################################################

    module HMAC
      HMAC_MAX_MD_CBLOCK = 128

      # Wrap HMAC_CTX
      #--
      #   typedef struct hmac_ctx_st {
      #     const EVP_MD *md;
      #     EVP_MD_CTX md_ctx;
      #     EVP_MD_CTX i_ctx;
      #     EVP_MD_CTX o_ctx;
      #     unsigned int key_length;
      #     unsigned char key[HMAC_MAX_MD_CBLOCK];
      #   } HMAC_CTX;
      #++
      class HMAC_CTX < FFI::Struct
        layout(:md,     :pointer,
               :md_ctx, EVP_MD_CTX,
               :i_ctx,  EVP_MD_CTX,
               :o_ctx,  EVP_MD_CTX,
               :key_length, :uint,
               :key,        [:uchar, HMAC_MAX_MD_CBLOCK])

        # Calls HMAC_CTX_init
        def initialize
          LibCrypto::HMAC_CTX_init(self)
        end
      end
    end

    #--
    # unsigned char *HMAC(const EVP_MD *evp_md,
    #                     const void *key,
    #                     int key_len,
    #                     const unsigned char *d,
    #                     size_t n,
    #                     unsigned char *md,
    #                     unsigned int *md_len)
    #++
    #
    # HMAC() computes the message authentication code of the n bytes at d
    # using the hash function evp_md and the key key which is key_len bytes
    # long.
    #
    # It places the result in md (which must have space for the output of
    # the hash function, which is no more than EVP_MAX_MD_SIZE bytes). If
    # md is NULL, the digest is placed in a static array. The size of the
    # output is placed in md_len, unless it is NULL.
    #
    # evp_md can be EVP_sha1(), EVP_ripemd160() etc.
    attach_function(:HMAC,
                    [:pointer, :const_string, :int, :const_string, :size_t,
                     :pointer, :pointer],
                    :string)
    #--
    # void HMAC_CTX_init(HMAC_CTX *ctx);
    #++
    # HMAC_CTX_init() initialises a HMAC_CTX before first use. It must be
    # called.  This is called by HMAC_CTX::initialize when an FFI HMAC_CTX
    # struct is created.
    attach_function(:HMAC_CTX_init, [:pointer], :void)

    #--
    # int HMAC_Init(HMAC_CTX *ctx, const void *key, int key_len,
    #               const EVP_MD *md);
    #++

    #--
    # int HMAC_Init_ex(HMAC_CTX *ctx, const void *key, int key_len,
    #                   const EVP_MD *md, ENGINE *impl);
    #++
    # HMAC_Init_ex() initializes or reuses a HMAC_CTX structure to use the
    # function evp_md and key key. Either can be NULL, in which case the
    # existing one will be reused. HMAC_CTX_init() must have been called
    # before the first use of an HMAC_CTX in this
    # function. N.B. HMAC_Init() had this undocumented behaviour in
    # previous versions of OpenSSL - failure to switch to HMAC_Init_ex() in
    # programs that expect it will cause them to stop working.
    #
# TODO: make key a :string?
    attach_function(:HMAC_Init_ex,
                    [:pointer, :pointer, :int, :pointer, :pointer], :int)

    #--
    # int HMAC_Update(HMAC_CTX *ctx, const unsigned char *data, int len);
    #++
    # HMAC_Update() can be called repeatedly with chunks of the message to
    # be authenticated (len bytes at data).
# TODO: Try changing all the HMAC_CTX* fields from :pointer to HMAC_CTX
# TODO: make data a :string
    attach_function(:HMAC_Update, [:pointer, :string, :int], :int)

    #--
    # int HMAC_Final(HMAC_CTX *ctx, unsigned char *md, unsigned int *len);
    #++
    #
    # HMAC_Final() places the message authentication code in md, which must
    # have space for the hash function output.
    #
    attach_function(:HMAC_Final, [:pointer, :pointer, :pointer], :int)

    #--
    # void HMAC_CTX_cleanup(HMAC_CTX *ctx);
    #++

    #--
    # void HMAC_cleanup(HMAC_CTX *ctx);
    #++

    #-- ############################################################
    #   Initialize
    #++ ############################################################

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
