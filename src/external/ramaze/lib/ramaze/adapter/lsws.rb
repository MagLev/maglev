#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  Global.test_connections = false

  module Adapter
    # Our Lsws adapter acts as wrapper for the Rack::Handler::LSWS.
    class Lsws < Base

      # start Lsws in a new thread, host and port parameter are only taken
      # to make it compatible with other adapters but have no influence and
      # can be omitted
      def self.startup(host = nil, port = nil)
        Thread.new{ Rack::Handler::LSWS.run(self) }
      end
    end
  end
end
