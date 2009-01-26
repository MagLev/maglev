#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'erubis'

module Ramaze
  module Template

    # Is responsible for compiling a template using the Erubis templating engine.
    # Can be found at: http://rubyforge.org/projects/erubis

    class Erubis < Template

      ENGINES[self] = %w[ rhtml ]

      # Allow for swapping in a differnt Erubis engine (it supplies several) and
      # passing options to the engine.
      trait[:engine] ||= ::Erubis::Eruby
      trait[:options] ||= {}

      class << self

        # Entry-point for Action#render

        def transform action
          eruby = wrap_compile(action)
          eruby.result(action.binding)
        end

        # Creates an instance of ::Erubis::Eruby, sets the filename
        # from the template and returns the instance.

        def compile(action, template)
          eruby = ancestral_trait[:engine].new(template, ancestral_trait[:options])
          eruby.init_evaluator(:filename => (action.template || __FILE__))
          eruby
        end
      end
    end
  end
end
