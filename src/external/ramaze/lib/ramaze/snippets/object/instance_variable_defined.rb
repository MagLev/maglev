#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for Object
    module Object
      # Available in 1.8.6 and later.

      unless ::Object.method_defined?(:instance_variable_defined?)
        def instance_variable_defined?(variable)
          instance_variables.include?(variable.to_s)
        end
      end
    end

  end
end
