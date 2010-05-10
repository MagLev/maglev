# Stub out enough of open ssl for rails
module OpenSSL
  class OpenSSLError < StandardError; end
end

require 'maglev/openssl/digest'
require 'maglev/openssl/hmac'
