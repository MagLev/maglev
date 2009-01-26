#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'markaby'

module Ramaze
  module Template

    # Is responsible for compiling a template using the Markaby templating engine.
    # Can be found at: http://code.whytheluckystiff.net/markaby/

    class Markaby < Template
      include Ramaze::Helper::Methods

      ENGINES[self] = %w[ mab ]

      class << self

        # Entry point for Action#render

        def transform action
          result, file = result_and_file(action)

          result = transform_string(file, action) if file
          result.to_s
        end

        # Takes a string and action, instance_evals the string inside a mab
        # block that gets the instance_variables of the original
        # action.instance passed.

        def transform_string string, action
          instance = action.instance
          ivs = extract_ivs(instance)

          instance.send(:mab, ivs) do
            instance_eval(string)
          end
        end

        # Generate a hash from instance-variables

        def extract_ivs(controller)
          controller.instance_variables.inject({}) do |hash, iv|
            sym = iv.gsub('@', '').to_sym
            hash.merge! sym => controller.instance_variable_get(iv)
          end
        end
      end
    end
  end
end
