require 'thin'
require 'rack/handler/thin'

module Ramaze
  module Adapter
    class Thin < Base
      # start server on given host and port.
      def self.startup(host, port)
        @server = ::Thin::Server.new(host, port, self)
        ::Thin::Logging.silent = true
        @server.timeout = 3

        Thread.new{ @server.start }
      end
    end
  end
end
