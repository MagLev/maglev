#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'nagoro'

module Ramaze
  module Template

    # Is responsible for compiling a template using the Nagoro templating engine.

    class Nagoro < Template

      ENGINES[self] = %w[ xhtml nag ]

      PIPES = ::Nagoro::DEFAULT_PIPES.dup

      class << self

        # Transforms an action into the XHTML code for parsing and returns
        # the result
        def transform action
          nagoro = wrap_compile(action)
          file = action.template || action.method
          nagoro.result(:file => file, :binding => action.binding)
        end

        # Use file_or_result instead of reaction_or_file
        def wrap_compile(action, template = nil)
          template ||= file_or_result(action)
          caching_compile(action, template)
        end

        def file_or_result(action)
          result = render_method(action).to_s

          if file = action.template
            return File.new(file)
          end

          result
        end

        # Compile a template, applying all transformations from the pipeline
        # and returning an instance of ::Nagoro::Template

        def compile(action, template)
          ::Nagoro.compile(template)
        end
      end
    end
  end
end
