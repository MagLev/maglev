#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for String

    module String
      unless ''.respond_to?(:end_with?)
        # Compatibility with 1.9
        def end_with?(other)
          other = other.to_s
          self[-other.size, other.size] == other
        end
      end
    end

  end
end
