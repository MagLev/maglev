# Stub out enough of open ssl for rails
require 'maglev/openssl/ffi/libcrypto'

# This module wraps the OpenSSL library for Ruby.
#
# The MagLev implementation does not have all the functions the MRI version
# has.  Some known limitations.  See Documentation for OpenSSL::Random.
module OpenSSL
  # The version of the ruby OpenSSL library
  VERSION = '1.0.0'

  # The version of the OpenSSL library used (text)
  OPENSSL_VERSION = 'OpenSSL 0.9.8j 07 Jan 2009'

  # The version of the OpenSSL library used (numeric)
  OPENSSL_VERSION_NUMBER = "0x009080af".hex

  # Generic error, common for all classes under OpenSSL module.
  class OpenSSLError < StandardError; end

  # call-seq:
  #   OpenSSL.debug -> true | false
  #
  # Returns the value of the OpenSSL debug flag.  See #debug= for details.
  def self.debug
    @debug
  end

  # call-seq:
  #   OpenSSL.debug = boolean -> boolean
  #
  # Turns on or off CRYPTO_MEM_CHECK.
  # Also shows some debugging message on stderr.
  # Returns the possibly updated value of @debug.
  def self.debug=(val)
    if val != @debug
      if @debug
        # TODO
        # CRYPTO_mem_ctrl(CRYPTO_MEM_CHECK_ON)
        puts "OSSL_DEBUG: IS NOW ON!"
      else
        # TODO
        # CRYPTO_mem_ctrl(CRYPTO_MEM_CHECK_OFF)
        puts "OSSL_DEBUG: IS NOW OFF!"
      end
    end
    @debug
  end
  @debug = false


  # call-seq:
  #   OpenSSL.errors -> [String...]
  #
  # See any remaining errors held in queue. Any errors you see here are
  # probably due to a bug in ruby's OpenSSL implementation.
  def self.errors
    # TODO
    #
    # while ((e = ERR_get_error()) != 0){
    #   rb_ary_push(ary, rb_str_new2(ERR_error_string(e, NULL)));
    # }
    ['OpenSSL.errors is not yet implemented...']
  end
end

require 'maglev/openssl/digest'
require 'maglev/openssl/hmac'
require 'maglev/openssl/random'
require 'maglev/openssl/ssl'
