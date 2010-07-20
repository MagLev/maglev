# Required for compatibility with obsolete: require 'digest/md5'
require 'digest'

class ::Digest::SHA384 < ::Digest::Base
  def initialize(str = nil)
    super(OpenSSL::Digest.const_get('SHA384').new(str))
  end

  private
  def self.digest_name
    'SHA384'
  end
end
