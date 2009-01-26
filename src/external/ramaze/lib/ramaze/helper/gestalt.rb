require 'gestalt'

module Ramaze
  module Helper
    module Gestalt
      def gestalt(&block)
        Ramaze::Gestalt.new(&block)
      end

      def build(&block)
        Ramaze::Gestalt.build(&block)
      end
    end
  end
end
