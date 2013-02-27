require File.expand_path('simple', File.dirname(__FILE__))

require 'openssl'

$md_algorithms = %w(MD4 MD5 RIPEMD160 SHA SHA1
                      SHA224 SHA256 SHA384 SHA512 DSS1)

def test_basic_ruby_use_case
  digest = OpenSSL::Digest.const_get('SHA1').new
  secret = 'duh'
  data = 'some secret data'
  hex = OpenSSL::HMAC.hexdigest(digest, secret, data)
  test(hex, "3107ffa818b9c5352860910ef7d259152f9baed8", 'hexdigest')
end

def test_hmac_reset
  digest = OpenSSL::Digest.const_get('SHA1').new
end

def test_all_hmac_impls
  secret = 'duh'
  data = 'some secret data'
  expected = {
    'SHA1'      => "3107ffa818b9c5352860910ef7d259152f9baed8",
    'MD2'       => "6eefda2c12c56068aa8f01ac208acc71",
    'MD4'       => "407a1ea0cd436cb576f76466363ea0d2",
    'MD5'       => "ee1673fb62b595c9d3eb152c7fd910ef",
    'MDC2'      => "1beb4b8bb5b401f8f7aebe17d32d2fef",
    'RIPEMD160' => "c4405a7f019da35571b6d4a260486f1ce6ce4dfe",
    'SHA'       => "f69a6ff233cdf7cc225591df5be6aa25469ccbd9",
    'SHA224'    => "b37410dc0a987bc15ee067c6c1ac63edf7e6f24c43a65473a267c836",
    'SHA256'    => "0f908ba6d41f5c4efde57086137c67abe2202d0422c18f35f480b558b9d6b659",
    'SHA384'    => "a9d6dac0095ccb8adeab243fb4b6fff88482e74b73dc363b595a3c93224ef445dac996041c132bb3aebc39a406921f43",
    'SHA512'    => "562a12945349bdb76bc13e4b4590f15bfb1416d7b17f1d180fb84470a1889a57c97b9abbc38dea8ac4731829dd570a033aff6a3b7a7d3e3033ace97f1e6e0197",
    'DSS1'      => "3107ffa818b9c5352860910ef7d259152f9baed8"
  }

  $md_algorithms.each do |md_type|
    digest = OpenSSL::Digest.const_get(md_type).new
    actual = OpenSSL::HMAC.hexdigest(digest, secret, data)
    test(actual, expected[md_type], "hmac algorithm #{md_type}")
  end
end

def test_crypto_digest
  $md_algorithms.each do |name|
    begin
      md = OpenSSL::Digest.new(name)
      test(md.nil?, false, "OpenSSL::Digest.new(#{name.inspect})")
      if name == "DSS1"
        name = "DSA" # this inconsistency is also on Rubinius
      end
      test(md.name, name, "#{name} .name")
    rescue RuntimeError => e
      p e
    end
  end

  md = OpenSSL::Digest::SHA1.new
  test(md.nil?, false, "OpenSSL::Digest::SHA1.new")
end

def test_file
  digest_class = OpenSSL::Digest.const_get('MD5')
  path = File.join(File.dirname(__FILE__), 'test_data', 'openssl_test.txt')

  # Test instance side method
  digest = digest_class.new.update('XYZ')
  digest.file(path)
  test(digest.hexdigest, "05c3a23c39f8212d5d8b0beca20cc09f", 'a_digest.file(...)')

  # Test class side method
  digest = digest_class.file(path)
  test(digest.hexdigest, "4d54ce1fbbf79e3f47f97732afdbe043", 'digest_class.file(...)')
end

def test_openssl_module
  test(OpenSSL::VERSION, '1.1.0', 'VERSION')
  test(OpenSSL.debug, false, 'Default value of debug')
end

def test_use_md_specific_classes
  digest = OpenSSL::Digest.const_get('SHA1').new("f")
  digest.update('f')
  test(digest.hexdigest, "ed70c57d7564e994e7d5f6fd6967cea8b347efbc", "Specific Digest hexdigest");
end

def test_use_md_unspecific_class
  digest = OpenSSL::Digest.new("sha1", "ff")
  digest.update('f')
  test(digest.hexdigest, "f6949a8c7d5b90b4a698660bbfb9431503fbb995", "Generic Digest hexdigest")
end

def test_random
  r = OpenSSL::Random
  test(Module, OpenSSL::Random.class, 'OpenSSL::Random is a module')

  str = "foo"
  test(OpenSSL::Random.seed(str), str, 'OpenSSL::Random.seed returns its argument')
  test(OpenSSL::Random.random_add(str, 0.3), OpenSSL::Random, 'OpenSSL::Random.random_add return value')
  len = 27
  rbytes = OpenSSL::Random.random_bytes(len)
  test(rbytes.length, len, "OpenSSL::Random.random_bytes: should be #{len} bytes")

  len = 134
  rbytes = OpenSSL::Random.pseudo_bytes(len)
  test(rbytes.length, len, "OpenSSL::Random.pseudo_bytes: should be #{len} bytes")

  random_enough = OpenSSL::Random.status?
  test(random_enough, true, 'OpenSSL::Random.status?')
end

def test_cipher
  # encryption
  encipher = OpenSSL::Cipher.new('aes-256-cbc')
  encipher.encrypt
  key = encipher.random_key
  iv = encipher.random_iv

  cipher_text = ''
  cipher_text += encipher.update("hello world")
  cipher_text += encipher.final

  test_not(cipher_text, "hello world", 'OpenSSL::Cipher encipher')

  # decryption
  decipher = OpenSSL::Cipher.new('aes-256-cbc')
  decipher.decrypt
  decipher.key = key
  decipher.iv = iv

  plain_text = ''
  plain_text += decipher.update(cipher_text)
  plain_text += decipher.final

  test(plain_text, "hello world", 'OpenSSL::Cipher decipher')
end

test_crypto_digest
test_basic_ruby_use_case
test_use_md_unspecific_class
test_use_md_specific_classes
test_openssl_module
test_all_hmac_impls
test_random
test_file
test_cipher

report
