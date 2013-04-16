
module OpenSSL
  class BNError < OpenSSLError; end

  class BN
    include Comparable
  end
end

class Integer
  def to_bn
    OpenSSL::BN::new(self.to_s(16), 16)
  end
end
