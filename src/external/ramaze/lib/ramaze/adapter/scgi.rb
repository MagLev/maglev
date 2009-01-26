#          Copyright (c) 2008 Jeremy Evans  code@jeremyevans.net
# All files in this distribution are subject to the terms of the Ruby license.

require 'rack/handler/scgi'

module Ramaze
  module Adapter
    # Our Scgi adapter acts as wrapper for the Rack::Handler::SCGI.
    class Scgi < Base
      # start SCGI in a new thread
      def self.startup(host, port)
        Thread.new do
          Rack::Handler::SCGI.run(self, :Host => host, :Port => port)
        end
      end
    end
  end
end
