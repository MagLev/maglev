#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for Kernel

    module Object

      # Original from Trans (Facets 1.4.5)
      # This is similar to +Module#const_get+ but is accessible at all levels,
      # and, unlike +const_get+, can handle module hierarchy.
      #
      #   constant("Fixnum")                  # -> Fixnum
      #   constant(:Fixnum)                   # -> Fixnum
      #
      #   constant("Process::Sys")            # -> Process::Sys
      #   constant("Regexp::MULTILINE")       # -> 4
      #
      #   require 'test/unit'
      #   Test.constant("Unit::Assertions")   # -> Test::Unit::Assertions
      #   Test.constant("::Test::Unit")       # -> Test::Unit

      def constant(const)
        const = const.to_s.dup

        if const.sub!(/^::/, '')
          base = Object
        elsif self.kind_of?(Module)
          base = self
        else
          base = self.class
        end

        const.split(/::/).inject(base){ |mod, name| mod.const_get(name) }
      end
    end

  end
end
