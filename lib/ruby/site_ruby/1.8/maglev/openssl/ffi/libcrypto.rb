module OpenSSL
  # FFI wrapper for the OpenSSL libcrypto library.  This provides access to
  # Message Digests (SHA1), etc.
  #
  # The library is initialized by calling OpenSSL_add_all_digests when this
  # file is loaded.  This registers all known Message Digest algorithms
  # (SHA1, MD5, etc.).
  class LibCrypto
    extend FFI::Library

    ffi_lib '$GEMSTONE/lib/libcrypto'  # $GEMSTONE expansion at runtime in VM prims

    # Creates and initializes a new <tt>EVP_MD_CTX</tt> struct for
    # <tt>digest_name</tt>, which should be one of the recognized digest
    # formats, e.g., "SHA1", "MD5", "SHA512".
    def self.evp_md_ctx_for_digest(digest_name)
      ctx = digest_alloc
      message_digest = OpenSSL::LibCrypto.EVP_get_digestbyname(digest_name)
      OpenSSL::LibCrypto.EVP_DigestInit_ex(ctx, message_digest, nil)
      ctx
    end

    # A +Proc+ that does finalization on message digest contexts.
    #
    # Note: MagLev finalizers get passed the object to be finalized, not
    # the object id.
    FINALIZER = Proc.new do |o|
      OpenSSL::LibCrypto.EVP_MD_CTX_destroy(o)
    end

    # Allocate, initialize and return an EVP_MD_CTX context.  Registers a
    # finalizer to destroy the context.  Raises a RuntimeError if can't
    # allocate the context.
    def self.digest_alloc
      cptr = OpenSSL::LibCrypto.EVP_MD_CTX_create
      raise 'EVP_MD_CTX_create() failed' if cptr.null?
      ctx = OpenSSL::LibCrypto::EVP_MD_CTX.new(cptr)

      ObjectSpace.define_finalizer(ctx, FINALIZER)
      ctx
    end

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
             :required_pkey_type,  [:char, 5],
             :block_size,          :int,
             :ctx_size,            :int)

      # Return the size of the message digest
      def md_size
        self[:md_size]
      end

      # Return the block size for the current message digest
      def block_size
        self[:block_size]
      end
    end

    # Wraps the <tt>EVP_MD_CTX</tt> struct that holds the message digest
    # information for a digest object.  Provides many of the interface
    # functions for a +Digest+.
    #--
    # struct env_md_ctx_st
    #   {
    #   const EVP_MD *digest;
    #   ENGINE *engine; /* functional reference if 'digest' is ENGINE-provided */
    #   unsigned long flags;
    #   void *md_data;
    #   EVP_PKEY_CTX *pctx;
    #  int (*update)(EVP_MD_CTX *ctx,const void *data,size_t count);
    #   } /* EVP_MD_CTX */;
    #++
    class EVP_MD_CTX < FFI::Struct
      layout(:digest, :pointer,
             :engine, :pointer,
             :flags,  :ulong,
             :data,   :pointer,
             :pctx,   :pointer,
             :updatefn, :pointer)
      # Return the message digest pointer.  Equivalent to
      # <tt>EVP_MD_CTX_md(ctx)</tt>.

      # Create, initialize and return a new EVP_MD_CTX
      def self.new_initialized_ctx
        ctx = new
        LibCrypto::EVP_MD_CTX_init(ctx)
        ctx
      end

      def initialize(*args)
        super
        @valid = true
      end

      # Return the number of bytes long the current digest is.
      def digest_length
        check_valid
        md.md_size
      end

      # Return the block size for the message digest
      def block_size
        check_valid
        md.block_size
      end

      # Return the EVP_MD struct used by receiver.
      def md
        check_valid
        @md ||= EVP_MD.new(self[:digest])
        @md
      end

      # Implement <tt>Digest#reset</tt>.
      # Reset the digest by calling <tt>EVP_DigestInit_ex</tt>, preserving
      # the message digest type.  Returns +self+.
      def reset
        check_valid
        OpenSSL::LibCrypto.EVP_DigestInit_ex(self, self.md, nil)
        self
      end

      def clone
        check_valid
        klone = OpenSSL::LibCrypto::EVP_MD_CTX.new_initialized_ctx
        OpenSSL::LibCrypto.EVP_MD_CTX_copy_ex(klone, self)
        klone
      end

      # Implement <tt>Digest#update</tt>.  Adds +string+ to the digested
      # data. Returns +self+.
      def update(string)
        check_valid
        string_ptr = FFI::MemoryPointer.from_string(string)
        OpenSSL::LibCrypto.EVP_DigestUpdate(self, string_ptr, string.length)
        self
      end

      # Implement <tt>Digest#finish</tt>.  Returns the digest string
      def finish
        check_valid
        d_len = self.md.md_size
        string_ptr = FFI::MemoryPointer.new(:char, d_len)
        OpenSSL::LibCrypto.EVP_DigestFinal_ex(self, string_ptr, nil)
        string_ptr.read_string(d_len)
      end

      # Calls EVP_MD_CTX_cleanup on self.  Illegal to use after this is called.
      def cleanup
        check_valid
        OpenSSL::LibCrypto.EVP_MD_CTX_cleanup(self)
        @valid = false
      end

      private
      def check_valid
        raise "Method called after cleanup on #{self.class.name}" unless @valid
      end
    end

    #--
    # int EVP_MD_CTX_copy_ex(EVP_MD_CTX *out,const EVP_MD_CTX *in);
    #++
    # Used to copy the message digest state from in to out. This is useful
    # if large amounts of data are to be hashed which only differ in the
    # last few bytes. out must be initialized before calling this function.
    # +libcrypto+ uses this to implement non-destructive reads of current
    # digest state.
    attach_function(:EVP_MD_CTX_copy_ex, [:pointer, :pointer], :int)

    #--
    #  int EVP_MD_CTX_cleanup(EVP_MD_CTX *ctx);
    #++
    # EVP_MD_CTX_cleanup() cleans up digest context ctx, it should be
    # called after a digest context is no longer needed.
    attach_function(:EVP_MD_CTX_cleanup, [EVP_MD_CTX], :int)

    #--
    # void EVP_MD_CTX_init(EVP_MD_CTX *ctx);
    #++
    # Initializes the digest context
    attach_function(:EVP_MD_CTX_init, [EVP_MD_CTX], :void)

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
    #--
    # TODO: make key a :string?
    attach_function(:HMAC_Init_ex,
                    [:pointer, :pointer, :int, :pointer, :pointer], :int)

    #--
    # int HMAC_Update(HMAC_CTX *ctx, const unsigned char *data, int len);
    #++
    # HMAC_Update() can be called repeatedly with chunks of the message to
    # be authenticated (len bytes at data).
    #
    #--
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
    #                           Random impl
    #++ ############################################################

    #--
    # void RAND_seed(const void *buf,int num);
    #++
    attach_function(:RAND_seed, [:const_string, :int], :void)

    #--
    # void RAND_add(const void *buf,int num,double entropy);
    #++
    attach_function(:RAND_add, [:const_string, :int, :double], :void)

    #--
    # int  RAND_bytes(unsigned char *buf,int num);
    #++
    attach_function(:RAND_bytes, [:pointer, :int], :int)

    #--
    # int  RAND_pseudo_bytes(unsigned char *buf,int num);
    #++
    attach_function(:RAND_pseudo_bytes, [:pointer, :int], :int)

    #--
    # int  RAND_status(void);
    #++
    attach_function(:RAND_status, [], :int)

    #-- ############################################################
    #   Initialize
    #++ ############################################################

    # Initialize libcrypto.  This performs the following, one-time
    # initialization calls:
    # + OpenSSL_add_all_digests    # Make SHA1 etc. available
    def self.initialize_library
      self.OpenSSL_add_all_digests
    end

    #--
    # void OpenSSL_add_all_digests(void)
    #++
    #
    # Loads all of the digests known to the libopenssl and makes them
    # available.  Must be called before EVP_get_digestbyname etc.
    attach_function(:OpenSSL_add_all_digests, [], :void).ffi_library.initialize_on_load {
      # this block will execute each time the library is loaded even if
      # this file is persistently loaded only once .
      LibCrypto.initialize_library
    }   
  end

end
