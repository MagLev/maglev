#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'amrita2'

class Amrita2::Template

  # Ramaze helpers are available in template contexts.

  include Ramaze::Helper::Methods
  helper :link, :sendfile, :flash, :cgi
end

module Ramaze
  module Template

    # Is responsible for compiling a template using the Amrita2 templating engine.
    # Can be found at: http://rubyforge.org/projects/amrita2

    class Amrita2 < Template

      ENGINES[self] = %w[ amrita amr a2html ]

      class << self

        # Takes an Action
        # The result or file is rendered using Amrita2::Template.
        #
        # The context data are set to @data in the controller before expansion.

        def transform(action)
          template = wrap_compile(action)
          data = action.instance.instance_variable_get("@data") || {}
          action.instance.extend ::Amrita2::Runtime if data.kind_of? Binding
          template.render_with(data)
        end

        # Compile a template, return instance of Amrita2::Template
        def compile(action, template)
          ::Amrita2::Template.new(template)
        end
      end
    end
  end
end
