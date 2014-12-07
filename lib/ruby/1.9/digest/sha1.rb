# Required for compatibility with obsolete: require 'digest/sha1'
require 'digest'

class ::Digest::SHA1 < ::Digest::Base
  def self.sha1(string)
    new(string)
  end

  def initialize(str = nil)
    super(OpenSSL::Digest.const_get('SHA1').new(str))
  end

  private
  def self.digest_name
    'SHA1'
  end
end
