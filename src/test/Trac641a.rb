
module Haml
  class Engine
    def render
      # buffer = 99
      begin
        scope_object = Object.new
        def scope_object.methx
          @haml_iv
        end
        buffer = "foo"
      ensure
        # If the following line is uncommented, then MagLev works.  I think
        # referencing the variable in this scope initializes something...
        #
        scope_object.instance_eval do
          @haml_iv = buffer
        end
        x = scope_object.methx  # MRI gets "foo"   MagLev gets "nil"
        unless x == 'foo' ; raise 'error'; end
      end
    end
  end
end

Haml::Engine.new.render
true
