#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/template/ezamar/engine'
require 'ramaze/template/ezamar/render_partial'

module Ramaze
  module Template

    # Is responsible for compiling a template using the Ezamar templating engine.

    class Ezamar < Template

      ENGINES[self] = %w[ xhtml zmr ]

      TRANSFORM_PIPELINE = [ ::Ezamar::Element ]

      class << self

        # Transforms an action into the XHTML code for parsing and returns
        # the result
        def transform action
          ezamar = wrap_compile(action)
          ezamar.result(action.binding)
        end

        # Compile a template, applying all transformations from the pipeline
        # and returning an instance of ::Ezamar::Template

        def compile(action, template)
          file = (action.template || __FILE__)

          TRANSFORM_PIPELINE.each do |tp|
            template = tp.transform(template)
          end

          ::Ezamar::Template.new(template, :file => file)
        end
      end
    end
  end
end
