#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'digest/sha1'

module Ramaze

  # A simple way to do authentication. Please have a look at the docs for the
  # check_auth method for detailed information

  module Helper::Auth
    Helper::LOOKUP << self

    # add Helper::Aspect and Helper::Stack on inclusion of Helper::Auth

    def self.included(klass)
      klass.send(:helper, :aspect, :stack)
    end

    # The default Element to use (if any)

    AUTH_ELEMENT = 'Page'

    # action for login, takes a password
    # ( ?password=passwort or /login/passwort or via a form )
    # if no password given, shows a simple form to input it.

    def login
      username, password = request[:username, :password]
      if check_auth(username, password)
        session[:logged_in] = true
        session[:username] = username
        inside_stack? ? answer : redirect_referrer
      else
        if defined? AUTH_ELEMENT and AUTH_ELEMENT.to_s.split.any?
          open_element = "<#{AUTH_ELEMENT}>"
          close_element = "</#{AUTH_ELEMENT}>"
        end
        %{
          #{open_element}
            <form method="POST" action="#{Rs(:login)}">
              <ul style="list-style:none;">
                <li>Username: <input type="text" name="username" /></li>
                <li>Password: <input type="password" name="password" /></li>
                <li><input type="submit" /></li>
              </ul>
            </form>
          #{close_element}
        }
      end
    end

    # clear the session and redirect to the index action of the mapping of the
    # current controller.

    def logout
      session.clear
      redirect_referer
    end

    private

    # call( R(self, :login) ) if not logged in

    def login_required
      call(Rs(:login)) unless logged_in?
    end

    # checks if the user is already logged in.
    #   session[:logged_in] is not nil/false

    def logged_in?
      !!session[:logged_in]
    end

    # check the authentication (username and password) against the auth_table.
    #
    # auth_table is a trait, it may be the name of a method, a Proc or a simple
    # Hash.
    # If it is neither of the above it has at least to respond to #[]
    # which will pass it the username as key and it should answer with the
    # password as a Digest::SHA1.hexdigest.
    #
    # The method and Proc are both called on demand.
    #
    # If you want to change the way the password is hashed, change
    #   trait[:auth_hashify]
    #
    # The default looks like:
    #   lambda{ |pass| Digest::SHA1.hexdigest(pass.to_s) }
    #
    # As with the auth_table, this has to be an object that responds to #[]
    #
    # If you want all your controllers to use the same mechanism set the trait
    # on one of the ancestors, the traits are looked up by #ancestral_trait
    #
    # Examples:
    #
    #   # The method to be called.
    #   trait :auth_table => :auth_table
    #   trait :auth_table => 'auth_table'
    #
    #   # Lambda that will be called upon demand
    #   trait :auth_table => lambda{ {'manveru' => Digest::SHA1.hexdigest 'password'} }
    #
    #   # Hash holding the data.
    #   trait :auth_table => {'manveru' => Digest::SHA1.hexdigest('password')}

    def check_auth user, pass
      return false if (not user or user.empty?) and (not pass or pass.empty?)
      auth_table = ancestral_trait[:auth_table] ||= {}

      auth_table = method(auth_table) if auth_table.is_a?(Symbol)
      auth_table = method(auth_table) if auth_table.respond_to?(:to_str)
      auth_table = auth_table.call    if auth_table.respond_to?(:call)

      default_hashify = lambda{ |pass| Digest::SHA1.hexdigest(pass.to_s) }

      hashify  = (ancestral_trait[:auth_hashify] ||= default_hashify)
      password = hashify[pass.to_s]

      auth_table[user.to_s] == password
    end
  end
end
