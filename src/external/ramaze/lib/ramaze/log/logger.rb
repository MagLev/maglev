#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'logger'

module Ramaze
  module Logger

    # Informer for the Stdlib Logger

    class Logger < ::Logger

      # integration to Logging

      def log(tag, *args)
        __send__(tag, args.join("\n"))
      end

      # Stub for compatibility
      def dev(*args)
        debug(*args)
      end
    end

  end
end
