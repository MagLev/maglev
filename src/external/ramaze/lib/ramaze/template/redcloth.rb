require 'redcloth'

module Ramaze
  module Template

    # Is responsible for compiling a template using the RedCloth templating engine.
    # Can be found at: http://whytheluckystiff.net/ruby/redcloth/

    class RedCloth < Erubis
      ENGINES[self] = %w[ redcloth ]

      class << self
        # Take the action and directly transform it into html by RedCloth
        def transform(action)
          restrictions = action.controller.trait[:redcloth_options] || []
          rules = action.controller.trait[:redcloth_options] || []

          # Erubis -> RedCloth -> HTML
          redcloth = ::RedCloth.new(super, restrictions)
          redcloth.to_html(*rules)
        end
      end
    end
  end
end
