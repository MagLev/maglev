=begin
= $RCSfile$ -- Generator for Makefile

= Info
  'OpenSSL for Ruby 2' project
  Copyright (C) 2002  Michal Rokos <m.rokos@sh.cvut.cz>
  All rights reserved.

= Licence
  This program is licenced under the same licence as Ruby.
  (See the file 'LICENCE'.)

= Version
  $Id: extconf.rb 32230 2011-06-26 01:32:03Z emboss $
=end

require "mkmf"

module GetOpenSSLHeaders
  class LibSsl
    extend FFI::Library
    ffi_lib Maglev::System.gs_lib_name(:SSL)
    attach_function( :SSLeay_version, [:int], :string )
  end

  # from openssl/crypto.h
  # #define SSLEAY_VERSION          0

  # SSLEAY_VERSION
  # The text variant of the version number and the release date. For example,
  # "OpenSSL 0.9.5a 1 Apr 2000".

  SSLEAY_VERSION = 0
  SSLEAY_VERSION_REGEX = /^(\w+) ((\d+)\.(\d+)\.(\d+)(.?)) (.+)$/

  extend self

  def parse_version_string(version_string)
    # name, version_id, major, minor, patch, status, timestamp
    version_id = version_string.match(SSLEAY_VERSION_REGEX)[2]
    version_id
  end

  def version
    parse_version_string(LibSsl.SSLeay_version(SSLEAY_VERSION))
  end

  def get_headers(version)
    unless File.directory?("openssl-#{version}")
      file = "openssl-#{version}.tar.gz"
      openssl_url = "ftp://ftp.openssl.org/source/#{file}"
      message " Downloading #{openssl_url}\n"
      message " Trying wget.\n"
      system "wget --quiet '#{openssl_url}'"
      unless $?.success?
        message " Trying curl.\n"
        system "curl -s -O '#{openssl_url}'"
        unless $?.success?
          raise "Must have curl or wget"
        end
      end
      system "tar xzf #{file}"
    end
  end

  def go!
    version_id = version
    get_headers(version_id)
    @@include_dir = "#{File.dirname(__FILE__)}/openssl-#{version_id}"
  end
  def include_dir
    @@include_dir
  end
end

module EnsureMaglevSSLLib
  extend self
  def go!
    # these are defaults.
    @@library = Maglev::System.gs_lib_name(:SSL)
    if @@library =~ /\$GEMSTONE\/lib\//
      @@library.gsub!('$GEMSTONE/lib/', '')
    end
    if @@library =~ /^lib/
      @@library.gsub!(/^lib/, '')
    end
    if @@library =~ /\.dylib$/
      @@library.gsub!(/\.dylib$/, '')
    end

    have_library( @@library, "SSLeay")

  end

  def library
    @@library
  end
end

message "=== OpenSSL for Ruby configurator ===\n"

message "=== Retrieving OpenSSL headers for MagLev version ===\n"
GetOpenSSLHeaders::go!

dir_config("openssl", GetOpenSSLHeaders::include_dir)
dir_config("kerberos")

EnsureMaglevSSLLib::go!

##
# Adds -Wall -DOSSL_DEBUG for compilation and some more targets when GCC is used
# To turn it on, use: --with-debug or --enable-debug
#
if with_config("debug") or enable_config("debug")
  $defs.push("-DOSSL_DEBUG") unless $defs.include? "-DOSSL_DEBUG"

  if CONFIG['GCC'] == 'yes'
    $CPPFLAGS += " -Wall" unless $CPPFLAGS.split.include? "-Wall"
  end
end

message "=== Checking for system dependent stuff... ===\n"

unless RbConfig::CONFIG["host_os"] =~ /^darwin/i
  # Seems not to be required on Darwin, and even breaks the build
  # sometimes
  have_library("nsl", "t_open")
  have_library("socket", "socket")
end
have_header("assert.h")

message "=== Checking for required stuff... ===\n"
if $mingw
  have_library("wsock32")
  have_library("gdi32")
end

result = have_header("openssl/ssl.h")

unless result
  result = have_header("openssl/ssl.h")
  result &&= %w[crypto libeay32].any? {|lib| have_library(lib, "OpenSSL_add_all_digests")}
  result &&= %w[ssl ssleay32].any? {|lib| have_library(lib, "SSL_library_init")}
  unless result
    message "=== Checking for required stuff failed. ===\n"
    message "Makefile wasn't created. Fix the errors above.\n"
    exit 1
  end
end

unless have_header("openssl/conf_api.h")
  message "OpenSSL 0.9.6 or later required.\n"
  exit 1
end

# have_func("rb_str_set_len", "ruby.h")
have_func("rb_block_call", "ruby.h")


message "=== Checking for OpenSSL features... ===\n"
have_func("ERR_peek_last_error")
have_func("ASN1_put_eoc")
have_func("BN_mod_add")
have_func("BN_mod_sqr")
have_func("BN_mod_sub")
have_func("BN_pseudo_rand_range")
have_func("BN_rand_range")
have_func("CONF_get1_default_config_file")
have_func("EVP_CIPHER_CTX_copy")
have_func("EVP_CIPHER_CTX_set_padding")
have_func("EVP_CipherFinal_ex")
have_func("EVP_CipherInit_ex")
have_func("EVP_DigestFinal_ex")
have_func("EVP_DigestInit_ex")
have_func("EVP_MD_CTX_cleanup")
have_func("EVP_MD_CTX_create")
have_func("EVP_MD_CTX_destroy")
have_func("EVP_MD_CTX_init")
have_func("HMAC_CTX_cleanup")
have_func("HMAC_CTX_copy")
have_func("HMAC_CTX_init")
have_func("PEM_def_callback")
have_func("PKCS5_PBKDF2_HMAC")
have_func("PKCS5_PBKDF2_HMAC_SHA1")
have_func("X509V3_set_nconf")
have_func("X509V3_EXT_nconf_nid")
have_func("X509_CRL_add0_revoked")
have_func("X509_CRL_set_issuer_name")
have_func("X509_CRL_set_version")
have_func("X509_CRL_sort")
have_func("X509_NAME_hash_old")
have_func("X509_STORE_get_ex_data")
have_func("X509_STORE_set_ex_data")
have_func("OBJ_NAME_do_all_sorted")
have_func("SSL_SESSION_get_id")
have_func("SSL_SESSION_cmp")
have_func("OPENSSL_cleanse")
have_func("SSLv2_method")
have_func("SSLv2_server_method")
have_func("SSLv2_client_method")
unless have_func("SSL_set_tlsext_host_name", ['openssl/ssl.h'])
  have_macro("SSL_set_tlsext_host_name", ['openssl/ssl.h']) && $defs.push("-DHAVE_SSL_SET_TLSEXT_HOST_NAME")
end
if have_header("openssl/engine.h")
  have_func("ENGINE_add")
  have_func("ENGINE_load_builtin_engines")
  have_func("ENGINE_load_openbsd_dev_crypto")
  have_func("ENGINE_get_digest")
  have_func("ENGINE_get_cipher")
  have_func("ENGINE_cleanup")
  have_func("ENGINE_load_4758cca")
  have_func("ENGINE_load_aep")
  have_func("ENGINE_load_atalla")
  have_func("ENGINE_load_chil")
  have_func("ENGINE_load_cswift")
  have_func("ENGINE_load_nuron")
  have_func("ENGINE_load_sureware")
  have_func("ENGINE_load_ubsec")
end
if checking_for('OpenSSL version is 0.9.7 or later') {
    try_static_assert('OPENSSL_VERSION_NUMBER >= 0x00907000L', 'openssl/opensslv.h')
  }
  have_header("openssl/ocsp.h")
end
have_struct_member("EVP_CIPHER_CTX", "flags", "openssl/evp.h")
have_struct_member("EVP_CIPHER_CTX", "engine", "openssl/evp.h")
have_struct_member("X509_ATTRIBUTE", "single", "openssl/x509.h")

message "=== Checking done. ===\n"

# don't acutally link against GemStone's libssl, but rather
# resolve-on-load
$libs.gsub!("-l#{EnsureMaglevSSLLib::library}","")

create_header
create_makefile("openssl")
message "Done.\n"
