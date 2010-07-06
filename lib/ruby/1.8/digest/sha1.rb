# TODO Move shared implementation with md5 up to Digest::Base
require 'digest'
require 'openssl'

class ::Digest::SHA1 < ::Digest::Base
  def self.sha1(*args)
    new(*args)
  end

  def initialize(str = nil)
    @impl = OpenSSL::Digest.const_get('SHA1').new(str)
#    @impl = OpenSSL::HMAC.new('', 'SHA1')
    self
  end

  def block_length
    @impl.block_length
  end

  def digest_length
    @impl.digest_length
  end

  alias :length :size

  def finish
    @impl.finish
  end

  def reset
    @impl.reset
  end

  def update(arg)
    @impl.update(arg)
  end
  alias :<< :update

  private
  def to_str(bignum)
    l = digest_length
    bytes = Array.new(l)
    b = bignum
    l.times do |i|
      bytes[l - i - 1] =  b & 0xFF
      b = b >> 8
    end
    bytes.pack("C*")
 end
end
