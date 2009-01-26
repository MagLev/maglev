#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for Array

    module Array

      #   a = [1, 2, 3]
      #   a.put_within(4, :after => 2, :before => 3)
      #   a # => [1, 2, 4, 3]

      def put_within(object, constrain)
        pre, post = constrain.values_at(:after, :before)

        return put_after(pre, object) if rindex(post) - index(pre) == 1

        raise ArgumentError, "Too many elements within constrain"
      end

      #   a = [1, 2, 3]
      #   a.put_after(2, 4)
      #   a # => [1, 2, 4, 3]

      def put_after(element, object)
        return self[index(element) + 1, 0] = object if include?(element)

        raise ArgumentError, "The given element doesn't exist"
      end

      #   a = [1, 2, 3]
      #   a.put_before(2, 4)
      #   a # => [1, 4, 2, 3]

      def put_before(element, object)
        return self[rindex(element), 0] = object if include?(element)

        raise ArgumentError, "The given element doesn't exist"
      end
    end
  end
end
