#          Copyright (c) 2009 Michael Fellinger m.fellinger@gmail.com
#          Copyright (c) 2008 rob@rebeltechnologies.nl
# All files in this distribution are subject to the terms of the Ruby license.

require 'syslog'

# Add aliases for the levelnames used by Ramaze logging
module Syslog
  alias dev debug
  alias warn warning
  alias error err
  module_function :dev, :warn, :error
end

module Ramaze
  module Logger
    # Logger class for writing to syslog. It is a *very* thin wrapper
    # around the Syslog library.
    class Syslog
      include Logging

      # Open the syslog library, if it is allready open, we reopen it using the
      # new argument list.  The argument list is passed on to the Syslog library
      # so please check that, and man syslog for detailed information.
      # There are 3 parameters:
      #
      #   ident:  The identification used in the log file, defaults to $0
      #   options:  defaults to  Syslog::LOG_PID | Syslog::LOG_CONS
      #   facility: defaults to Syslog::LOG_USER
      #
      def initialize( *args )
        ::Syslog.close if ::Syslog.opened?
        ::Syslog.open( *args )
      end

      # just sends all messages received to ::Syslog
      # We simply return if the log was closed for some reason, this behavior
      # was copied from Informer.  We do not handle levels here. This will
      # be done by te syslog daemon based on it's configuration.
      def log(tag, *messages)
        return if !::Syslog.opened?
        ::Syslog.send(tag, *messages)
      end

      # Has to call the modules singleton-method.
      def inspect
        ::Syslog.inspect
      end
    end
  end
end
