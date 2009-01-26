#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'sass/engine'

module Ramaze
  module Template

    # Is responsible for compiling a template using the Sass CSS templating engine.
    # Can be found at: http://haml.hamptoncatlin.com/

    class Sass < Template

      ENGINES[self] = %w[ sass ]

      class << self

        # Transform via Sass templating engine

        def transform action
          if response = Response.current
            response['Content-Type'] = "text/css"
          end
          sass = wrap_compile(action)
          sass.to_css()
        end

        # Instantiates Sass::Engine with the template and sass_options trait from
        # the controller.

        def compile(action, template)
          ::Sass::Engine.new(template, action.controller.trait[:sass_options] || {})
        end
      end
    end
  end
end
