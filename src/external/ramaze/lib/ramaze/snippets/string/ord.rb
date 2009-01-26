#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for String

    module String
      unless ''.respond_to?(:ord)

        # compatibility with Ruby 1.9

        def ord
          self[0]
        end
      end
    end

  end
end
