#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for String

    module String
      # 1.9 Compatibility with 1.8

      unless ''.respond_to?(:each)
        def each(*args, &block)
          each_line(*args, &block)
        end
      end
    end
  end
end
