# Required for compatibility with obsolete: require 'digest/md5'
require 'digest'

class ::Digest::SHA256 < ::Digest::Base
  def initialize(str = nil)
    super(OpenSSL::Digest.const_get('SHA256').new(str))
  end

  private
  def self.digest_name
    'SHA256'
  end
end
