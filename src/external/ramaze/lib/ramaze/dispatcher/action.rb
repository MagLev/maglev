#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Dispatcher

    # This dispatcher is responsible for relaying requests to Controller::handle
    # and filtering the results using FILTER.

    class Action

      # The response is passed to each filter by sending .call(response) to it.

      FILTER = OrderedSet[
        # Ramaze::Tool::Localize,
      ] unless defined?(FILTER)

      class << self
        include Trinity

        # Takes path, asks Controller to handle it and builds a response on
        # success. The response is then passed to each member of FILTER for
        # post-processing.

        def call(path)
          log(path)

          catch(:respond) {
            response.write Controller.handle(path)
          }

          FILTER.each{|f| f.call(response)}
          response
        rescue Ramaze::Error => ex
          ex
        end

        # Logs the request via Log#info unless it's boring.

        def log(path)
          case path
          when *Global.boring
          else
            Log.info("Dynamic request from #{request.ip}: #{request.request_uri}")
          end
        end
      end
    end
  end
end
