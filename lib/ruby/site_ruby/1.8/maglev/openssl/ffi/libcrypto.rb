module OpenSSL

  # Wraps the OpenSSL libcrypto library.  This provides access to Message
  # Digests (SHA1), etc.
  #
  # The library is initialized when this file is loaded
  class LibCrypto
    extend FFI::Library

    # TODO: dev builds are messed up and add .dylib.dylib on end
    ffi_lib "#{ENV['GEMSTONE']}/lib/libcrypto.0.9.8.dylib"

    attach_function(:OpenSSL_add_all_digests, [], :void)
    attach_function(:EVP_get_digestbyname, [:string], :pointer)

    attach_function(:EVP_MD_CTX_create, [], :pointer)
    attach_function(:EVP_MD_CTX_destroy, [:pointer], :void)

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
