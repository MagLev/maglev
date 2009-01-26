module Ramaze
  module CoreExtensions

    # Extensions for Binding

    module Binding

      # Returns a hash of localvar/localvar-values from binding, useful for
      # template engines that do not accept bindings and force passing locals
      # via hash
      #
      # Usage:
      #   x = 42; p binding.locals #=> {'x'=> 42}

      def locals
        ::Kernel::eval '
        local_variables.inject({}){|h,v|
          k = v.to_s
          h.merge!(k => eval(k))
        }', self
      end

    end
  end
end
