#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for String

    module String

      # simple transformation to CamelCase from snake_case
      #
      #   'foo_bar'.camel_case # => 'FooBar'

      def camel_case
        split('_').map{|e| e.capitalize}.join
      end
    end

  end
end
