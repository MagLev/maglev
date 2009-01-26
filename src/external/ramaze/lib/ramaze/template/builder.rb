require 'builder'

module Ramaze
  module Template

      class Builder < Template
        ENGINES[self] = %w[ builder rxml ]

        class << self
          def transform action
            if response = Response.current
              response['Content-Type'] = "application/xml"
            end

            template = wrap_compile(action)
            action.instance.instance_eval(template, action.template || __FILE__)
          end

          def compile action, template
            "xml = ::Builder::XmlMarkup.new(:indent => 2)\n" +
            template +
            "\nxml.target!\n"
          end
        end
      end

  end
end