require 'ebb'

module Ramaze
  module Adapter
    class Ebb < Base
      def self.startup(host, port)
        ::Ebb.log = StringIO.new
        Thread.new do
          ::Ebb.start_server(self, :port => port)
        end
      end

      def self.shutdown
        ::Ebb.stop_server
      end
    end
  end
end
