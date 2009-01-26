require "tenjin"
require "tempfile"

# Tenjin does not work with a Binding, so helpers included by the controller
# will not be automatically available in templates. Instead we add support
# for popular helpers to its internal Context class to make them available.
class Tenjin::Context
  include Ramaze::Helper::Methods
  helper :link, :sendfile, :flash, :cgi
end

module Ramaze
  module Template

    # Is responsible for compiling a template using the Tenjin templating engine.
    # Can be found at: http://www.kuwata-lab.com/tenjin/
    #
    # Instance variables in your action method are automatically available in
    # the template:
    #
    #  class MainController < Ramaze::Controller
    #    engine :Tenjin
    #    def index
    #      @names = ['Alice', 'Bob', 'Charlie']
    #      @greeting = 'Hello'
    #      %q{
    #      <html><body>
    #        <h1><?rb for name in @names ?><p>#{@greeting} #{name}</p><?rb end ?></h1>
    #      </body></html>
    #      }
    #    end
    #  end
    #
    # Note that only instance vars are available and methods (including helper methods)
    # are not available in the template. A few common helpers are made generally available
    # though by virtue of the modifications to Tenjin::Context at the top of this file.

    class Tenjin < Template

      ENGINES[self] = %w[ rbhtml tenjin ]

      class << self
        # Transforms an action into the XHTML code for parsing and returns
        # the result.
        def transform(action)
          tenjin = wrap_compile(action)
          # Tenjin can't deal with a Binding so we construct a hash for it.
          tenjin.render(context(action))
        end

        # Return a hash of all instance variables from the action.
        # For each instance var @foo, we put :foo => @foo in the hash.
        def context action
          hash = {}
          action_binding = action.binding
          var_names = action.instance.instance_variables
          var_names.each do |var_name|
            # Strip the @ from the front of the var name to use as hash key,
            # then simply use the value of the instance var as the hash value.
            hash[var_name[1..-1]] = eval(var_name, action_binding)
          end
          hash
        end

        # Compile a template, returning an instance of ::Tenjin::Template.
        def compile(action, template)
          tenjin = ::Tenjin::Template.new
          tenjin.convert(template)
          return tenjin
        end
      end
    end
  end
end
