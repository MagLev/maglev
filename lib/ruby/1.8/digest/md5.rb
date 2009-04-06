require 'digest'
class ::Digest::MD5 < ::Digest::Base
  def self.md5(*args)
    new(*args)
  end

  def intialize(str = nil, *rest)
    @data = str
  end

  def digest(string, *rest)
    return string._md5sum
  end
end
