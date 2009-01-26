#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'mongrel'
require 'rack/handler/mongrel'

module Ramaze
  module Adapter

    # Our Mongrel adapter acts as wrapper for the Rack::Handler::Mongrel.
    class Mongrel < Base

      # start server on given host and port.
      def self.startup(host, port)
        @server = ::Mongrel::HttpServer.new(host, port)
        @server.register('/', ::Rack::Handler::Mongrel.new(self))
        @server.run
      end
    end
  end
end
