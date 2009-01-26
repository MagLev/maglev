#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'maruku'

module Ramaze
  module Template

    # Is responsible for compiling a template using the Ezamar templating engine.

    class Maruku < Template

      ENGINES[self] = %w[ mkd ]

      class << self

        # Transforms an action into the XHTML code for parsing and returns
        # the result
        def transform(action)
          maruku = wrap_compile(action)
          maruku.to_html
        end

        # Compile a template, applying all transformations from the pipeline
        # and returning an instance of ::Ezamar::Template

        def compile(action, template)
          ::Maruku.new(template)
        end
      end
    end
  end
end

