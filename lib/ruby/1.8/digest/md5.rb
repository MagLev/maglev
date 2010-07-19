# Required for compatibility with obsolete: require 'digest/md5'
require 'digest'

class ::Digest::MD5 < ::Digest::Base
  def self.md5(string)
    new(string)
  end

  def initialize(str = nil)
    super(OpenSSL::Digest.const_get('MD5').new(str))
  end

  private
  def self.digest_name
    'MD5'
  end
end
