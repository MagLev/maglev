module Ramaze
  module CoreExtensions

    # Extensions for Object
    module Object

      # Returns the string that #pretty_inspect would yield

      def pretty s = ''
        PP.pp(self, s)
        s
      end
    end

  end
end
