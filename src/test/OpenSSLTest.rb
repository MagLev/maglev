require File.expand_path('simple', File.dirname(__FILE__))

require 'openssl'

def test_basic_rb
  digest = OpenSSL::Digest.const_get('SHA1').new
  secret = 'duh'
  data = 'some secret data'
  p OpenSSL::HMAC.hexdigest(digest, secret, data)
end

def test_crypto_digest
  puts "test_digest"
  digest = OpenSSL::Digest.new
  digest = nil
end

def test_sha1
  bogus = OpenSSL::LibCrypto.EVP_get_digestbyname('BOGUS')
  test(bogus.null?, true, "EVP_get_digestbyname BOGUS")

  sha1 = OpenSSL::LibCrypto.EVP_get_digestbyname('SHA1')
  test(sha1.null?, false, "EVP_get_digestbyname SHA1")
end

test_sha1
# test_crypto_digest

report
