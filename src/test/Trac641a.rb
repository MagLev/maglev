
module Haml
  class Engine
    def render
      scope_object = Object.new
      def scope_object.buffer
        @haml_buffer
      end
      buffer = "foo"
      puts "main: " + buffer
    ensure
      # If the following line is uncommented, then MagLev works.  I think
      # referencing the variable in this scope initializes something...
      #
      #p buffer
      scope_object.instance_eval do
        @haml_buffer = buffer
      end
      p scope_object.buffer  # MRI prints "foo"   MagLev prints "nil"
    end
  end
end

Haml::Engine.new.render
