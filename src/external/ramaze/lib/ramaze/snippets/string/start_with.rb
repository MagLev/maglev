module Ramaze
  module CoreExtensions

    # Extensions for String

    module String

      unless ''.respond_to?(:start_with?)

        # Compatibility with 1.9

        def start_with?(other)
          other = other.to_s
          self[0, other.size] == other
        end
      end
    end
  end
end
