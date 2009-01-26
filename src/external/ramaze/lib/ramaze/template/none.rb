#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Template

      class None < Template
        def self.transform action
          render_method(action)
        end
      end

  end
end
