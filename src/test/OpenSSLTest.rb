require 'openssl'

digest = OpenSSL::Digest.const_get('SHA1').new
secret = 'duh'
data = 'some secret data'
p OpenSSL::HMAC.hexdigest(digest, secret, data)
