#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'liquid'

module Ramaze
  module Template

    # Is responsible for compiling a template using the Liquid templating engine.
    # Can be found at: http://home.leetsoft.com/liquid

    class Liquid < Template

      ENGINES[self] = %w[ liquid ]

      class << self

        # initializes the handling of a request on the controller.
        # Creates a new instances of itself and sends the action and params.
        # Also tries to render the template.
        # In Theory you can use this standalone, this has not been tested though.

        def transform action
          template = reaction_or_file(action)

          instance = action.instance
          hash     = instance.instance_variable_get("@hash") || {}
          template = ::Liquid::Template.parse(template)
          options  = instance.ancestral_trait[:liquid_options]

          template.render(hash, options)
        end
      end
    end
  end
end
