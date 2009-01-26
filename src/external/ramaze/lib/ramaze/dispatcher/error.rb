#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Dispatcher

    # Last resort dispatcher, tries to recover as much information as possible
    # from the past request and takes the appropiate actions.
    #
    # You can configure it over the HANDLE_ERROR constant or by defining error
    # actions in your controllers.

    class Error

      # The class of exception is matched when an error occurs and the status
      # code is set. The absolute URLs are used as fallback in case of a total
      # failure.
      HANDLE_ERROR = {
                          Exception => [ 500, '/error' ],
            Ramaze::Error::NoAction => [ 404, '/error' ],
        Ramaze::Error::NoController => [ 404, '/error' ],
      }

      class << self
        trait :last_error => nil

        # Takes exception, of metainfo only :controller is used at the moment.
        # Then goes on to try and find the correct response status and path.
        # In case metainfo has a controller we try to get the action for the
        # path on it, dispatching there if we find one.
        # Otherwise a plain-text error message is set as response.
        def call(error, metainfo = {})
          log_error(error)

          STATE[:exception] = error
          response = Response.current

          key = error.class.ancestors.find{|a| HANDLE_ERROR[a]}
          status, path = *HANDLE_ERROR[key || Exception]
          status ||= 500

          if controller = metainfo[:controller]
            newpath = (controller.mapping + path).squeeze('/')
            action_response = Dispatcher::Action.call(newpath)
            case action_response
            when Ramaze::Error
              Log.debug("No custom error page found on #{controller}, going to #{path}")
            else
              action_response.status = status
              return action_response
            end
          end

          if path and error.message !~ /`#{path}'/
            response.status = status
            return Dispatcher.dispatch_to(path)
          else
            response.build(error.message, status)
          end
        rescue Object => ex
          Log.error(ex)
          begin
            m = ex.message
            c = ex.class
            b = [ex.backtrace].flatten.join("\n")
            body = <<-html
            ==== Error ====\n
              #{ m } (#{ c })
              #{ b }\n
            ==== Request ====\n
              #{ Request.current.pretty }\n
            ==== Request ====\n
              #{ Response.current.pretty }\n
            ==== Session ====\n
              #{ Session.current.pretty }\n
            ==== Global ====\n
              #{ Global.pretty }\n
            html
            response['Content-Type'] = 'text/plain'
            response.build(body.unindent, status)
          rescue Object
            raise
          end
        end

        # Only logs new errors with full backtrace, repeated errors are shown
        # only with their message.
        def log_error error
          error_message = error.message

          if trait[:last_error] == error_message or error.is_a? Ramaze::Error::NoAction
            Log.error(error_message)
          else
            trait[:last_error] = error_message
            Log.error(error)
          end
        end

        # Handle to current exception.
        # Only works inside request/response cycle.
        def current
          STATE[:exception]
        end

      end
    end
  end
end
