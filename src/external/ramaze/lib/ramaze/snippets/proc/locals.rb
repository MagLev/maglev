module Ramaze
  module CoreExtensions

    # Extensions for Proc

    module Proc

      # returns a hash of localvar/localvar-values from proc, useful for template
      # engines that do not accept bindings/proc and force passing locals via
      # hash
      #   usage: x = 42; p Proc.new.locals #=> {'x'=> 42}

      def locals
        instance_eval('binding').locals
      end

    end
  end
end
