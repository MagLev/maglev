require 'digest'
class ::Digest::MD5 < ::Digest::Base
  def self.md5(*args)
    new(*args)
  end

  def initialize(str = nil, *rest)
    @data = str
    self
  end

  def block_length
    64
  end

  def digest_length
    16
  end

  alias :length :size

  def finish
    data = Type.coerce_to(@data, String, :to_s)
    data.__md5sum  # result is a hex string
  end

  def reset
    @data = nil
  end

  def update(arg)
    if @data.nil?
      @data = arg
    else
      @data << arg
    end
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
