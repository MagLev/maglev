#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

# This module serves as a namespace for all templates, it will autoload
# Amrita2, Erubis, Haml, Liquid and Markaby if you refer to them.

module Ramaze

  # Namespace for templating.

  module Template

    COMPILED = {} unless defined?(COMPILED)

    # Place register_engine puts the class and extensions for templating engines

    ENGINES = {} unless defined?(ENGINES)

    AVAILABLE_ENGINES = %w[
      Amrita2 Builder Erubis Haml Liquid Markaby Maruku Nagoro None RedCloth
      Remarkably Sass Tagz Tenjin XSLT
    ]

    AVAILABLE_ENGINES.each do |const|
      autoload(const, "ramaze/template/#{const.downcase}")
    end

    # The superclass for all templates, contains the shared behaviour of
    # the templates and includes Ramaze::Helper::Methods

    class Template
      include Ramaze::Helper::Methods

      class << self

        # calls result_and_file with the given action and returns the first of
        # the result of the controller or content of the file.

        def reaction_or_file action
          result_and_file(action).reverse.compact.first
        end

        # Takes an Action and returns the result from sending the action.method
        # to the controller via render_method and reads the contents of the file
        # if given.

        def result_and_file(action)
          result = render_method(action)

          if file = action.template
            content = File.read(file)
          end

          [result, content]
        end

        # returns nil if no method is on the action, otherwise it will send the
        # action and optional parameters to the controller via __send__ and
        # return the unaltered result

        def render_method(action)
          return unless method = action.method
          action.instance.__send__(method, *action.params)
        end

        # This is a wrapper to use Global.compile without even thinking about
        # it. Don't use it if your engine is not based on a compile/eval
        # principle.

        def wrap_compile(action, template = nil)
          template ||= reaction_or_file(action).to_s
          caching_compile(action, template)
        end

        # If Global.compile is enabled try to find the action in the cache,
        # otherwise give a new compiled object back.
        def caching_compile(action, template)
          if Global.compile && action.template
            Cache.compiled[action.relaxed_hash] ||= compile(action, template)
          else
            compile(action, template)
          end
        end
      end
    end
  end
end
