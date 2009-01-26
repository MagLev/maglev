#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # The general Namespace for Ramazes Errorclasses

  class Error < StandardError
    # No action found on Controller
    class NoAction < Error; end

    # No Controller found for request
    class NoFilter < Error; end

    # No Controller found for request
    class NoController < Error; end

    # Wrong parameter count for action
    class WrongParameterCount < Error; end

    # Error while transformation in template
    class Template < Error; end
  end
end
