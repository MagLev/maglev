# file timeout1.rb

module Timeout

  ##
  # Raised by Timeout#timeout when the block times out.

  class Error < Interrupt
    def self.name
      'Timeout::Error' # workaround prefix lost during bootstrap
    end
  end
  class ExitException < ::Exception # :nodoc:
    def self.name
      'Timeout::ExitException' # workaround prefix lost during bootstrap
    end
  end

  # Maglev,  these two constants not needed
  # THIS_FILE = /\A#{Regexp.quote(__FILE__)}:/o
  # CALLER_OFFSET = ((c = caller[0]) && THIS_FILE =~ c) ? 1 : 0

end
Timeout.__freeze_constants

##
# Another name for Timeout::Error, defined for backwards compatibility with
# earlier versions of timeout.rb.

TimeoutError = Timeout::Error # :nodoc:
Object.__freeze_constants

