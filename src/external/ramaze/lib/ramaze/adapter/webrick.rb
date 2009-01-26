#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'webrick'
require 'rack/handler/webrick'

module Ramaze
  module Adapter
    # Our WEBrick adapter acts as wrapper for the Rack::Handler::WEBrick.
    class WEBrick < Base
      OPTIONS = {
        :Logger      => Log,
        :AccessLog   => [
          [Log, ::WEBrick::AccessLog::COMMON_LOG_FORMAT],
          [Log, ::WEBrick::AccessLog::REFERER_LOG_FORMAT]
        ]
      }

      def self.startup(host, port)
        options = OPTIONS.merge(:BindAddress => host, :Port => port)
        @server = ::WEBrick::HTTPServer.new(options)
        @server.mount('/', ::Rack::Handler::WEBrick, self)
        Thread.new{ @server.start }
      end
    end
  end
end

# Extending on WEBrick module
module WEBrick

  # Extending on HTTPServlet
  module HTTPServlet

    # Extending on ProcHandler
    #
    # We alias PUT to GET and DELETE to GET so WEBrick handles them as well.
    class ProcHandler
      alias do_PUT do_GET
      alias do_DELETE do_GET
    end
  end
end
