require 'psych/ffi/libpsych'
require 'psych/ffi/emitter'
require 'psych/ffi/parser'

# Other items implemented in psych as a C extension

module Psych
  module Visitors
    class Visitor
    end

    class ToRuby < Visitor
      private
      def build_exception(klass, mesg)
        e = klass.new
        e.instance_variable_set(:mesg, mesg)
        e
      end

      def path2class(path)
        Maglev::RubyUtils.rb_path2class(path)
      end
    end
  end
end
