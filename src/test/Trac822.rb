# Another one from Tilt....
module Tilt
  module CompileSite
  end

  class Template
    def compile_template_method(method_name)
      CompileSite.class_eval <<-RUBY, 'xxx', 10
        def #{method_name}(locals)
          p locals
          unless locals == 330 ; raise 'Fail 1'; end
          class << self
            this, locals = Thread.current[:tilt_vars]
            p locals
            unless locals == 940 ; raise 'Fail 2;' end
            this.instance_eval do
	      p locals
              unless locals == 940 ; raise 'Fail 2;' end	
              raise "Fail: nil locals" if locals.nil?
            end
          end
	  p locals
          unless locals == 330 ; raise 'Fail 2;' end
        end
      RUBY
    end
  end
end

class App
  include Tilt::CompileSite    
end

t = Tilt::Template.new
t.compile_template_method('foo')  
a = App.new
Thread.current[:tilt_vars] = [a, 940 ]
a.foo(330)
true
