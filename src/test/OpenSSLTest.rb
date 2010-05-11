require File.expand_path('simple', File.dirname(__FILE__))

require 'openssl'

def test_basic_ruby_use_case
  digest = OpenSSL::Digest.const_get('SHA1').new
  secret = 'duh'
  data = 'some secret data'
  hex = OpenSSL::HMAC.hexdigest(digest, secret, data)
  test(hex, "3107ffa818b9c5352860910ef7d259152f9baed8", 'hexdigest')

end

def test_crypto_digest

  %w( DSS DSS1 MD2 MD4 MD5 MDC2 RIPEMD160
      SHA SHA1 SHA224 SHA256 SHA384 SHA512 ).each do |name|
    md = OpenSSL::Digest.new(name)
    test(md.nil?, false, "OpenSSL::Digest.new(#{name.inspect})")
    test(md.name, name, "#{name} .name")
  end

  md = OpenSSL::Digest::SHA1.new
  test(md.nil?, false, "OpenSSL::Digest::SHA1.new")
end

def test_sha1
  bogus = OpenSSL::LibCrypto.EVP_get_digestbyname('BOGUS')
  test(bogus.null?, true, "EVP_get_digestbyname BOGUS")

  sha1 = OpenSSL::LibCrypto.EVP_get_digestbyname('SHA1')
  test(sha1.null?, false, "EVP_get_digestbyname SHA1")
end

if defined? Maglev
  test_sha1
end

test_crypto_digest
test_basic_ruby_use_case

report
