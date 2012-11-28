# Required for compatibility with obsolete: require 'digest/md5'
require 'digest'

class ::Digest::SHA512 < ::Digest::Base
  def initialize(str = nil)
    super(OpenSSL::Digest.const_get('SHA512').new(str))
  end

  private
  def self.digest_name
    'SHA512'
  end
end
