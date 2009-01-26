module Ramaze
  module Helper

    # This helper provides a convenience wrapper for handling authentication
    # and persistence of users.
    #
    # Example:
    #
    # class MainController < Ramaze::Controller
    #   def index
    #     if logged_in?
    #       "Hello #{user.name}"
    #     else
    #       A('login', :href => Rs(:login))
    #     end
    #   end
    #
    #   def login
    #     user_login if reuqest.post?
    #   end
    # end

    module User
      # return existing or instantiate User::Wrapper
      def user
        model = ancestral_trait[:user_model] ||= ::User
        callback = ancestral_trait[:user_callback] ||= nil
        STATE[:user] ||= Wrapper.new(model, callback)
      end

      # shortcut for user.user_login but default argument are request.params

      def user_login(creds = request.params)
        user._login(creds)
      end

      # shortcut for user.user_logout

      def user_logout
        user._logout
      end

      def logged_in?
        user._logged_in?
      end

      # Wrapper for the ever-present "user" in your application.
      # It wraps around an arbitrary instance and worries about authentication
      # and storing information about the user in the session.
      #
      # In order to not interfere with the wrapped instance/model we start our
      # methods with an underscore.
      # Suggestions on improvements as usual welcome.
      class Wrapper < BlankSlate
        attr_accessor :_model, :_callback, :_user

        def initialize(model, callback)
          @_model, @_callback = model, callback
          @_user = nil
          _login
        end

        def _login(creds = _persistence)
          if @_user = _would_login?(creds)
            self._persistence = creds
          end
        end

        # The callback should return an instance of the user, otherwise it
        # should answer with nil.
        #
        # This will not actually login, just check whether the credentials
        # would result in a user.
        def _would_login?(creds)
          if c = @_callback
            c.call(creds)
          elsif _model.respond_to?(:authenticate)
            _model.authenticate(creds)
          else
            Log.warn("Helper::User has no callback and there is no %p::authenticate" % _model)
            nil
          end
        end

        def _logout
          _persistence.clear
          STATE[:user] = nil
        end

        def _logged_in?
          !!_user
        end

        def _persistence=(creds)
          Current.session[:USER] = creds
        end

        def _persistence
          Current.session[:USER] || {}
        end

        # Refer everything not known
        def method_missing(meth, *args, &block)
          return unless _user
          _user.send(meth, *args, &block)
        end
      end
    end
  end
end
