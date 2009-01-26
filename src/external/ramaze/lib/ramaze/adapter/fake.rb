module Ramaze
  module Adapter
    class Fake < Base
      def self.startup(host = nil, port = nil)
        Thread.new{ loop{ sleep(1) } }
      end

      def self.shutdown
      end
    end
  end
end
